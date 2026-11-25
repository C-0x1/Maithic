# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Networks and properties as documents, and the verdict on a property as one.
#
# A network document (schema `maithic.nnv.network/1`) lists layers; a property
# document (schema `maithic.nnv.property/1`) gives an input box and the box
# the outputs are claimed to lie in. Numbers are text, read exactly: `"3"`,
# `"-7/2"`, `"0.1"`. A JSON number is taken for the exact value of the
# floating-point number it denotes.

exact_number(value::AbstractString) = parse_exact(value)
exact_number(value::Integer) = Exact(value)
exact_number(value::AbstractFloat) = exact_value(value)
exact_number(value) = throw(ArgumentError("a number was expected, as text or as a JSON number, and $(repr(value)) was found"))

member(object, name::AbstractString, what::AbstractString) =
    object isa Object && haskey(object, name) ? object[name] : throw(ArgumentError("$(what) has no member \"$(name)\""))

function activation_of(name)
    name == "relu" && return ReLU
    name == "identity" && return Identity
    throw(ArgumentError("the activation $(repr(name)) is not known; the activations are \"relu\" and \"identity\""))
end

"""
    network_from_document(document)

Reads a network from a document of the schema `maithic.nnv.network/1`. The
network is exact: no number is rounded while it is read.
"""
function network_from_document(doc)
    expect_schema(doc, "maithic.nnv.network/1")
    layers = member(doc, "layers", "the network")
    layers isa AbstractVector && !isempty(layers) || throw(ArgumentError("the network has no layer"))
    width = nothing
    built = Layer{Exact}[]
    for (index, layer) in enumerate(layers)
        neurons = member(layer, "neurons", "layer $(index)")
        neurons isa AbstractVector && !isempty(neurons) || throw(ArgumentError("layer $(index) has no neuron"))
        row = Neuron{Exact}[]
        for neuron in neurons
            weights = Exact[exact_number(w) for w in member(neuron, "weights", "a neuron of layer $(index)")]
            width === nothing || length(weights) == width ||
                throw(ArgumentError("a neuron of layer $(index) has $(length(weights)) weight(s), and the layer before it has $(width) output(s)"))
            push!(row, Neuron{Exact}(weights, exact_number(member(neuron, "bias", "a neuron of layer $(index)"))))
        end
        width === nothing && (allequal(length(n.weights) for n in row) || throw(ArgumentError("the neurons of layer 1 differ in their number of weights")))
        push!(built, Layer{Exact}(row, activation_of(member(layer, "activation", "layer $(index)"))))
        width = length(row)
    end
    Network{Exact}(built)
end

function box_from(items, what::AbstractString)
    items isa AbstractVector && !isempty(items) || throw(ArgumentError("$(what) is empty"))
    box = Interval{Exact}[]
    for (index, item) in enumerate(items)
        item isa AbstractVector && length(item) == 2 || throw(ArgumentError("coordinate $(index) of $(what) is not a pair of bounds"))
        lower, upper = exact_number(item[1]), exact_number(item[2])
        lower <= upper || throw(ArgumentError("coordinate $(index) of $(what) has its lower bound above its upper bound"))
        push!(box, Interval{Exact}(lower, upper))
    end
    box
end

"""
    property_from_document(document)

Reads a property from a document of the schema `maithic.nnv.property/1`: the
input box and the box the outputs are claimed to lie in.
"""
function property_from_document(doc)
    expect_schema(doc, "maithic.nnv.property/1")
    (box_from(member(doc, "input", "the property"), "the input box"), box_from(member(doc, "output", "the property"), "the output box"))
end

"The network as a document of the schema `maithic.nnv.network/1`."
network_document(net::Network{Exact}) = document("maithic.nnv.network/1", "layers" => Any[layer_data(l) for l in net.layers])

"""
    verdict_document(net, input, claimed; max_corners = 1024)

Decides the property and writes down what was found, in the schema
`maithic.nnv.verdict/1`: `verified` with the boxes of a certificate,
`violated` with a point of the input box and its exact output, or `unknown`.
The document is a claim. `maithic nnv verify` has the checkers in Lean look
at the boxes or at the point before it reports anything.
"""
function verdict_document(net::Network{Exact}, input::Box{Exact}, claimed::Box{Exact}; max_corners::Integer = 1024)
    first_width = length(first(net.layers).neurons[1].weights)
    length(input) == first_width || throw(ArgumentError("the input box has $(length(input)) coordinate(s), and the network takes $(first_width)"))
    outputs = length(last(net.layers).neurons)
    length(claimed) == outputs || throw(ArgumentError("the output box has $(length(claimed)) coordinate(s), and the network gives $(outputs)"))
    verdict, evidence = decide_property(net, input, claimed; max_corners)
    common = Any["network" => Any[layer_data(l) for l in net.layers], "input" => box_data(input), "output" => box_data(claimed)]
    engine = "engine" => engine_record("MaithicNNV", VERSION, "method" => "interval bound propagation, outward rounding", "max_corners" => Int(max_corners))
    if verdict == Verified
        document("maithic.nnv.verdict/1", "verdict" => "verified", common..., "boxes" => Any[box_data(b) for b in evidence.boxes], engine)
    elseif verdict == Violated
        document("maithic.nnv.verdict/1", "verdict" => "violated", common...,
                 "counterexample" => Object("point" => Any[exact_text(x) for x in evidence], "output" => Any[exact_text(y) for y in evaluate(net, evidence)]), engine)
    else
        document("maithic.nnv.verdict/1", "verdict" => "unknown", common...,
                 "reason" => "the bounds are too loose to show the property, and no tested point of the input box leaves the claimed box", engine)
    end
end
