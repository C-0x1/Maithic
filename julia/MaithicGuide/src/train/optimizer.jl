# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Adam, with weight decay in both of its forms.

"""
    Adam(; rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_decay = 0.0, decoupled = true)

The Adam optimizer. With `decoupled = true` the weight decay is applied to
the weights directly, apart from the gradient, which is the method known as
AdamW; with `decoupled = false` it is added to the gradient, as in the
original Adam with L2 regularization.
"""
Base.@kwdef struct Adam
    rate::Float64 = 0.01
    beta1::Float64 = 0.9
    beta2::Float64 = 0.999
    epsilon::Float64 = 1e-8
    weight_decay::Float64 = 0.0
    decoupled::Bool = true
end

function check(optimizer::Adam)
    optimizer.rate > 0 || throw(ArgumentError("the learning rate must be above zero"))
    0 <= optimizer.beta1 < 1 && 0 <= optimizer.beta2 < 1 || throw(ArgumentError("beta1 and beta2 must be in [0, 1)"))
    optimizer.epsilon > 0 || throw(ArgumentError("epsilon must be above zero"))
    optimizer.weight_decay >= 0 || throw(ArgumentError("the weight decay must not be negative"))
    optimizer
end

"""
    AdamState(arrays)

What Adam remembers between steps for a list of arrays: the running means of
the gradients and of their squares, and the number of steps taken.
"""
mutable struct AdamState
    first::Vector{Array{Float64}}
    second::Vector{Array{Float64}}
    steps::Int
end

AdamState(arrays) = AdamState(Array{Float64}[zeros(size(a)) for a in arrays], Array{Float64}[zeros(size(a)) for a in arrays], 0)

"""
    step!(optimizer, state, arrays, gradients)

One step of Adam: changes each array in place by its gradient. An array of
one dimension is taken for a bias and is not decayed.
"""
function step!(optimizer::Adam, state::AdamState, arrays, gradients)
    length(arrays) == length(gradients) == length(state.first) || throw(ArgumentError("one gradient is needed for each array"))
    state.steps += 1
    correction1 = 1.0 - optimizer.beta1^state.steps
    correction2 = 1.0 - optimizer.beta2^state.steps
    for (k, array) in enumerate(arrays)
        size(array) == size(gradients[k]) || throw(ArgumentError("gradient $(k) does not have the shape of its array"))
        decay = ndims(array) >= 2 ? optimizer.weight_decay : 0.0
        first, second = state.first[k], state.second[k]
        @inbounds for i in eachindex(array)
            g = gradients[k][i]
            optimizer.decoupled || (g += decay * array[i])
            first[i] = optimizer.beta1 * first[i] + (1.0 - optimizer.beta1) * g
            second[i] = optimizer.beta2 * second[i] + (1.0 - optimizer.beta2) * g * g
            update = (first[i] / correction1) / (sqrt(second[i] / correction2) + optimizer.epsilon)
            optimizer.decoupled && (update += decay * array[i])
            array[i] -= optimizer.rate * update
        end
    end
    nothing
end
