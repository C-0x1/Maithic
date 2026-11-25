# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Deciding a property from bounds.

"The outcome of a bound-based decision."
@enum Verdict Verified Violated Unknown

"""
    decide_property(net, input, claimed)

Decides whether every input in the box `input` is mapped into the box
`claimed`.

- `Verified`: the propagated bounds lie inside the claimed box. The second
  value is the certificate that shows it. Bounds are computed in floating
  point first; when those are too loose and the network has at most
  `exact_limit` neurons, they are computed again in exact arithmetic, which
  costs more and loses nothing to rounding.
- `Violated`: one of the tested points of the input box is mapped outside the
  claimed box, in exact arithmetic. The second value is that point.
- `Unknown`: the bounds are too loose to decide and no tested point is a
  counterexample. The second value is `nothing`.

The tested points are the centre and the corners of the input box, the corners
only while there are at most `max_corners` of them.
"""
function decide_property(net::Network, input::AbstractVector{<:Interval}, claimed::AbstractVector{<:Interval};
                         max_corners::Integer = 1024, exact_limit::Integer = 256)
    exact_net = exact(net)
    box = enclose(input)
    target = enclose(claimed)
    certificate = certify(net, input)
    if check_certificate(certificate) && is_subset(final_box(certificate), target)
        return (Verified, certificate)
    end
    if sum(length(layer.neurons) for layer in exact_net.layers) <= exact_limit
        sharper = Certificate(exact_net, box, propagate_exact(net, input))
        if check_certificate(sharper) && is_subset(final_box(sharper), target)
            return (Verified, sharper)
        end
    end
    for point in test_points(box, max_corners)
        encloses(target, evaluate(exact_net, point)) || return (Violated, point)
    end
    (Unknown, nothing)
end

"The centre of a box and, when there are few enough, its corners."
function test_points(box::AbstractVector{Interval{Exact}}, max_corners::Integer)
    points = [[(i.lo + i.hi) / 2 for i in box]]
    n = length(box)
    if n <= 20 && 2^n <= max_corners
        for mask in 0:(2^n - 1)
            push!(points, [(mask >> (k - 1)) & 1 == 1 ? box[k].hi : box[k].lo for k in 1:n])
        end
    end
    points
end
