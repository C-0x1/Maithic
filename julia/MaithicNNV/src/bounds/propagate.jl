# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Interval bound propagation.

"The output box of one layer over an input box."
function bound(layer::Layer{T}, box::AbstractVector{Interval{T}}) where {T}
    [activate(layer.activation, shift(n.bias, dot_prefix(n.weights, box))) for n in layer.neurons]
end

"""
    layer_boxes(net, input)

The box after every layer, first to last, for inputs in the box `input`. The
number type of the network and of the box must agree.
"""
function layer_boxes(net::Network{T}, input::AbstractVector{Interval{T}}) where {T}
    boxes = Vector{Box{T}}()
    current = collect(input)
    for layer in net.layers
        current = bound(layer, current)
        push!(boxes, current)
    end
    boxes
end

"""
    propagate_exact(net, input)

Interval bound propagation in exact rational arithmetic. The result is the same
as `Maithic.NN.Network.bound` computes in Lean.
"""
propagate_exact(net::Network, input::AbstractVector{<:Interval}) =
    layer_boxes(exact(net), enclose(input))

"""
    propagate(net, input)

Interval bound propagation in `Float64` with outward rounding, returned as
exact rational boxes, one per layer.

The parameters are first rounded to `Float64`. To keep the result sound for
the *exact* network, each weight and bias is treated as an interval that
contains its exact value, and every arithmetic step is rounded outwards. The
boxes are therefore valid bounds for the exact network, and slightly wider
than what exact propagation gives.
"""
function propagate(net::Network, input::AbstractVector{<:Interval})
    exact_net = exact(net)
    current = [Interval{Float64}(float_down(i.lo), float_up(i.hi)) for i in enclose(input)]
    boxes = Vector{Box{Exact}}()
    for layer in exact_net.layers
        next = Vector{Interval{Float64}}()
        for n in layer.neurons
            acc = Interval{Float64}(0.0, 0.0)
            for k in 1:min(length(n.weights), length(current))
                acc = add(acc, mul(parameter_interval(n.weights[k]), current[k]))
            end
            acc = add(acc, parameter_interval(n.bias))
            push!(next, activate(layer.activation, acc))
        end
        current = next
        push!(boxes, enclose(current))
    end
    boxes
end

"A `Float64` at or below an exact value."
function float_down(x::Exact)
    f = Float64(x)
    Exact(f) <= x ? f : prevfloat(f)
end

"A `Float64` at or above an exact value."
function float_up(x::Exact)
    f = Float64(x)
    Exact(f) >= x ? f : nextfloat(f)
end

"The tightest floating-point interval that contains an exact parameter."
parameter_interval(x::Exact) = Interval{Float64}(float_down(x), float_up(x))

"The interval product, enclosing `x * y` for `x` in `a` and `y` in `b`."
function mul(a::Interval{Float64}, b::Interval{Float64})
    products = (a.lo * b.lo, a.lo * b.hi, a.hi * b.lo, a.hi * b.hi)
    Interval{Float64}(down(minimum(products)), up(maximum(products)))
end
