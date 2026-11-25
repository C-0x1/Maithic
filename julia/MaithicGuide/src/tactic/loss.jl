# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The loss of a classifier over tactic templates.

"""
    log_softmax(scores)

The logarithms of the probabilities that the scores stand for. Computed from
the largest score down, so that scores of any size give finite results.
"""
function log_softmax(scores::AbstractVector{Float64})
    top = maximum(scores)
    total = 0.0
    for s in scores
        total += exp(s - top)
    end
    scores .- (top + log(total))
end

softmax(scores::AbstractVector{Float64}) = exp.(log_softmax(scores))

"""
    cross_entropy(scores, target)

The loss of a prediction: minus the logarithm of the probability that the
scores give to the class `target`. Zero when the model is certain and right;
`log(n)` when it gives all `n` classes the same score.
"""
function cross_entropy(scores::AbstractVector{Float64}, target::Integer)
    1 <= target <= length(scores) || throw(ArgumentError("class $(target) is not one of the $(length(scores)) classes"))
    -log_softmax(scores)[target]
end

"""
    cross_entropy_gradient(scores, target)

The gradient of [`cross_entropy`](@ref) with respect to the scores: the
probabilities, less one at the target.
"""
function cross_entropy_gradient(scores::AbstractVector{Float64}, target::Integer)
    1 <= target <= length(scores) || throw(ArgumentError("class $(target) is not one of the $(length(scores)) classes"))
    gradient = softmax(scores)
    gradient[target] -= 1.0
    gradient
end
