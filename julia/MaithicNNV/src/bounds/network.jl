# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Typed network structures, parametric in the number type: exact rationals for
# semantics and certificates, floating point for fast bound computation.

"The activation applied to every output of a layer."
@enum Activation Identity ReLU

"One neuron: a weight row and a bias."
struct Neuron{T<:Real}
    weights::Vector{T}
    bias::T
end

Neuron(weights::AbstractVector, bias::Real) = begin
    T = promote_type(eltype(weights), typeof(bias))
    Neuron{T}(collect(T, weights), T(bias))
end

"One layer: its neurons and the activation applied to each output."
struct Layer{T<:Real}
    neurons::Vector{Neuron{T}}
    activation::Activation
end

"A feed-forward network: layers applied first to last."
struct Network{T<:Real}
    layers::Vector{Layer{T}}
end

"The exact rational type used for semantics and certificates."
const Exact = Rational{BigInt}

"""
    Layer(weights, biases, activation)

Builds a layer from a weight matrix with one row per neuron and a bias vector.
"""
function Layer(weights::AbstractMatrix{<:Real}, biases::AbstractVector{<:Real}, activation::Activation)
    size(weights, 1) == length(biases) || throw(DimensionMismatch("one bias per weight row is required"))
    T = promote_type(eltype(weights), eltype(biases))
    Layer{T}([Neuron{T}(collect(T, weights[i, :]), T(biases[i])) for i in axes(weights, 1)], activation)
end

Network(layers::Layer{T}...) where {T} = Network{T}(collect(layers))

"Converts every parameter of a network to another number type."
convert_network(::Type{T}, net::Network) where {T<:Real} =
    Network{T}([Layer{T}([Neuron{T}(T.(n.weights), T(n.bias)) for n in l.neurons], l.activation) for l in net.layers])

"""
    exact(net)

The same network with exact rational parameters. Floating-point parameters are
converted exactly: every finite `Float64` is a rational number.
"""
exact(net::Network) = convert_network(Exact, net)

"""
    floating(net)

The same network with `Float64` parameters, rounded to nearest. Bounds computed
from it are widened so that they still enclose the exact network; see
[`propagate`](@ref).
"""
floating(net::Network) = convert_network(Float64, net)

apply(a::Activation, x::Real) = a == ReLU ? max(zero(x), x) : x

"The dot product of the common prefix of two vectors."
function dot_prefix(w::AbstractVector, x::AbstractVector)
    n = min(length(w), length(x))
    acc = zero(promote_type(eltype(w), eltype(x)))
    for i in 1:n
        acc += w[i] * x[i]
    end
    acc
end

"""
    evaluate(net, x)

The output of the network on the input vector `x`. With an exact network and
exact input this is the reference semantics; with floating-point values it is
an approximation of it.
"""
function evaluate(net::Network, x::AbstractVector{<:Real})
    current = collect(x)
    for layer in net.layers
        current = [apply(layer.activation, dot_prefix(n.weights, current) + n.bias) for n in layer.neurons]
    end
    current
end

"The number of inputs the first layer reads, taken from its longest weight row."
input_size(net::Network) = isempty(net.layers) ? 0 : maximum(length(n.weights) for n in first(net.layers).neurons; init = 0)

"The number of outputs of the last layer."
output_size(net::Network) = isempty(net.layers) ? 0 : length(last(net.layers).neurons)
