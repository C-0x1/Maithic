# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# How often a model is right.

# The indices of the `k` largest scores, the largest first; equal scores in
# the order of their index, so that the answer does not depend on a sort.
function top_indices(scores::AbstractVector{<:Real}, k::Integer)
    order = sortperm(scores; rev = true, alg = MergeSort)
    order[1:min(k, length(order))]
end

"""
    top_templates(model, state, k = 5)

The `k` templates the model thinks most likely for a proof state, each with
its probability, the most likely first.
"""
function top_templates(model::GuideModel, state::AbstractString, k::Integer = 5)
    p = template_probabilities(model, state)
    [model.templates.templates[i] => p[i] for i in top_indices(p, k)]
end

"""
    Evaluation(examples, loss, top1, topk, k)

A model on a set of examples: the mean loss, and how often the right
template was the most likely one and among the `k` most likely.
"""
struct Evaluation
    examples::Int
    loss::Float64
    top1::Float64
    topk::Float64
    k::Int
end

"""
    evaluate(model, examples; k = 5)

The [`Evaluation`](@ref) of a model on examples it may or may not have seen.
An example whose tactic has no template of its own counts under the class
for all other tactics.
"""
function evaluate(model::GuideModel, examples; k::Integer = 5)
    k >= 1 || throw(ArgumentError("k must be at least 1"))
    isempty(examples) && return Evaluation(0, NaN, NaN, NaN, k)
    loss, first, among = 0.0, 0, 0
    for example in examples
        target = class_of(model.templates, example.tactic)
        z = template_scores(model, state_ids(model.vocabulary, example.state; max_length = model.config.max_length))
        loss += cross_entropy(z, target)
        best = top_indices(z, k)
        first += best[1] == target
        among += target in best
    end
    n = length(examples)
    Evaluation(n, loss / n, first / n, among / n, k)
end

"""
    topk_accuracy(model, examples, k)

The share of examples whose template is among the `k` the model thinks most
likely; with `k = 1`, how often the model is simply right.
"""
topk_accuracy(model::GuideModel, examples, k::Integer) = k == 1 ? evaluate(model, examples; k = 1).top1 : evaluate(model, examples; k = k).topk

"""
    baseline(templates, examples; k = 5)

What a model would score that knows no state and always answers with the
templates most frequent in training: the [`Evaluation`](@ref) to beat. The
loss is that of predicting the training frequencies.
"""
function baseline(templates::TemplateVocabulary, examples; k::Integer = 5)
    isempty(examples) && return Evaluation(0, NaN, NaN, NaN, k)
    total = sum(templates.counts)
    frequencies = [(count + 1) / (total + length(templates)) for count in templates.counts]
    best = top_indices(frequencies, k)
    loss, first, among = 0.0, 0, 0
    for example in examples
        target = class_of(templates, example.tactic)
        loss -= log(frequencies[target])
        first += best[1] == target
        among += target in best
    end
    n = length(examples)
    Evaluation(n, loss / n, first / n, among / n, k)
end
