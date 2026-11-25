# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Bound certificates: per-layer boxes with exact rational endpoints, in the
# shape that the Lean checker `Maithic.NN.checkCertificate` reads.

"""
    Certificate

Claimed bounds for a network on an input box: one box per layer. The boxes may
be looser than exact propagation, never tighter.
"""
struct Certificate
    network::Network{Exact}
    input::Box{Exact}
    boxes::Vector{Box{Exact}}
end

"""
    certify(net, input)

Computes a certificate with floating-point bound propagation. The result is
meant to be checked, by [`check_certificate`](@ref) here or by the verified
checker in Lean.
"""
certify(net::Network, input::AbstractVector{<:Interval}) = Certificate(exact(net), enclose(input), propagate(net, input))

"""
    check_certificate(certificate)

Checks a certificate in exact arithmetic, layer by layer, the way the Lean
checker does: the bounds recomputed from the previous box must lie inside the
claimed box, and checking continues from the claimed box.
"""
function check_certificate(c::Certificate)
    length(c.boxes) == length(c.network.layers) || return false
    current = c.input
    for (layer, claimed) in zip(c.network.layers, c.boxes)
        is_subset(bound(layer, current), claimed) || return false
        current = claimed
    end
    true
end

"The last box of a certificate: the claimed bounds on the output."
final_box(c::Certificate) = isempty(c.boxes) ? c.input : last(c.boxes)

# ---- JSON ----

interval_data(i::Interval{Exact}) = Any[exact_text(i.lo), exact_text(i.hi)]
box_data(b::AbstractVector{Interval{Exact}}) = Any[interval_data(i) for i in b]

layer_data(l::Layer{Exact}) =
    Object("activation" => (l.activation == ReLU ? "relu" : "identity"),
           "neurons" => Any[Object("weights" => Any[exact_text(w) for w in n.weights], "bias" => exact_text(n.bias))
                            for n in l.neurons])

"""
    certificate_document(certificate)

The certificate in the schema `maithic.nnv.certificate/1`. Rational numbers
are text of the form `"3"` or `"-7/2"`, so that no precision is lost in any
JSON reader.
"""
certificate_document(c::Certificate) =
    document("maithic.nnv.certificate/1", "network" => Any[layer_data(l) for l in c.network.layers],
             "input" => box_data(c.input), "boxes" => Any[box_data(b) for b in c.boxes])

"The JSON text of [`certificate_document`](@ref)."
certificate_json(c::Certificate; pretty::Bool = false) = write_json(certificate_document(c); pretty)

# ---- Lean source ----

lean_rational(x::Exact) = begin
    text = denominator(x) == 1 ? string(numerator(x)) : string(numerator(x), "/", denominator(x))
    x < 0 ? string("(", text, ")") : text
end
lean_interval(i::Interval{Exact}) = string("⟨", lean_rational(i.lo), ", ", lean_rational(i.hi), "⟩")
lean_box(b::AbstractVector{Interval{Exact}}) = string("[", join((lean_interval(i) for i in b), ", "), "]")

function lean_layer(l::Layer{Exact})
    neurons = join((string("⟨[", join((lean_rational(w) for w in n.weights), ", "), "], ", lean_rational(n.bias), "⟩")
                    for n in l.neurons), ", ")
    string("{ neurons := [", neurons, "], activation := .", l.activation == ReLU ? "relu" : "identity", " }")
end

"""
    certificate_lean(certificate; name = "cert")

Lean definitions of the network, the input box and the certificate, followed by
a `#guard` that runs the verified checker on them.
"""
function certificate_lean(c::Certificate; name::AbstractString = "cert")
    string("import Maithic.NN.Cert.Bounds\n\nopen Maithic.NN\n\n",
           "def ", name, "Net : Network := [\n  ", join((lean_layer(l) for l in c.network.layers), ",\n  "), "]\n\n",
           "def ", name, "Input : Box := ", lean_box(c.input), "\n\n",
           "def ", name, " : Certificate := [\n  ", join((lean_box(b) for b in c.boxes), ",\n  "), "]\n\n",
           "#guard checkCertificate ", name, "Net ", name, "Input ", name, "\n")
end
