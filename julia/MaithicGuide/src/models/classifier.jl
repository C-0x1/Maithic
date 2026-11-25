# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The whole model: tokens to vectors, vectors to one vector, one vector to a
# score for every tactic template.

const MIXERS = (:none, :attention)

"""
    ModelConfig(; dim = 32, max_length = 128, pooling = :mean, mixer = :none)

The shape of a model: the size of its vectors, the longest sequence it
reads, how it pools (`:mean` or `:first`), and whether a layer of attention
lets the tokens see each other before pooling (`:attention`) or not
(`:none`). A goal longer than `max_length` tokens is cut; the conclusion is
read first, so what is cut are hypotheses.
"""
Base.@kwdef struct ModelConfig
    dim::Int = 32
    max_length::Int = 128
    pooling::Symbol = :mean
    mixer::Symbol = :none
end

function check(config::ModelConfig)
    config.dim >= 1 || throw(ArgumentError("dim must be at least 1"))
    config.max_length >= 1 || throw(ArgumentError("max_length must be at least 1"))
    config.pooling in POOLINGS || throw(ArgumentError("there is no pooling called $(config.pooling); the poolings are $(join(POOLINGS, " and "))"))
    config.mixer in MIXERS || throw(ArgumentError("there is no mixer called $(config.mixer); the mixers are $(join(MIXERS, " and "))"))
    config
end

"""
    Head(weights, bias)

The classifier: a score for every class from the pooled vector,
`weights * h + bias`, with one row of weights for each class.
"""
struct Head
    weights::Matrix{Float64}
    bias::Vector{Float64}
end

Head(rng::SplitMix, classes::Integer, dim::Integer; deviation::Real = 0.1) = Head(normal_matrix(rng, classes, dim, deviation), zeros(classes))

scores(head::Head, h::AbstractVector{Float64}) = mul(head.weights, h) .+ head.bias

"""
    GuideModel

A tactic classifier with the vocabulary it reads and the templates it
predicts. `attention` is `nothing` when the model has no mixer.
"""
struct GuideModel
    config::ModelConfig
    vocabulary::Vocabulary
    templates::TemplateVocabulary
    embedding::Embedding
    attention::Union{Attention,Nothing}
    head::Head
end

"""
    GuideModel(config, vocabulary, templates; seed = 0)

A model with random weights. The weights are drawn from the stream of the
seed for `"weights"`, so a seed gives the same model wherever it is made.
"""
function GuideModel(config::ModelConfig, vocabulary::Vocabulary, templates::TemplateVocabulary; seed::Integer = 0)
    check(config)
    rng = stream(seed, "weights")
    embedding = Embedding(rng, length(vocabulary), config.dim, config.max_length)
    attention = config.mixer === :attention ? Attention(rng, config.dim) : nothing
    GuideModel(config, vocabulary, templates, embedding, attention, Head(rng, length(templates), config.dim))
end

"""
    parameters(model)

The arrays of weights of a model, in a fixed order, each with its name. An
optimizer changes them in place.
"""
function parameters(model::GuideModel)
    named = Pair{String,Array{Float64}}["tokens" => model.embedding.tokens, "positions" => model.embedding.positions]
    if model.attention !== nothing
        append!(named, ["query" => model.attention.query, "key" => model.attention.key, "value" => model.attention.value])
    end
    append!(named, ["weights" => model.head.weights, "bias" => model.head.bias])
    named
end

"Arrays of zeros in the shapes of the parameters: where gradients are added up."
zero_gradients(model::GuideModel) = Array{Float64}[zeros(size(array)) for (_, array) in parameters(model)]

# The pooled vector of a sequence and what the way back needs.
function encode_forward(model::GuideModel, ids::AbstractVector{<:Integer})
    x = embed(model.embedding, ids)
    if model.attention === nothing
        (pool(model.config.pooling, x), (x = x, mixed = x, cache = nothing))
    else
        mixed, cache = attend(model.attention, x)
        (pool(model.config.pooling, mixed), (x = x, mixed = mixed, cache = cache))
    end
end

"""
    template_scores(model, ids)

The score of every template for a sequence of token numbers.
"""
template_scores(model::GuideModel, ids::AbstractVector{<:Integer}) = scores(model.head, encode_forward(model, ids)[1])

"""
    template_probabilities(model, state)

The probability of every template for a proof state given as text. The
state is read as [`state_ids`](@ref) reads it.
"""
template_probabilities(model::GuideModel, state::AbstractString) =
    softmax(template_scores(model, state_ids(model.vocabulary, state; max_length = model.config.max_length)))

"""
    accumulate!(gradients, model, ids, target)

Adds to `gradients` the gradient of the loss of one example, a sequence of
token numbers with the class that is right for it, and returns the loss.
`gradients` has the shapes of [`zero_gradients`](@ref).
"""
function accumulate!(gradients::Vector{Array{Float64}}, model::GuideModel, ids::AbstractVector{<:Integer}, target::Integer)
    h, state = encode_forward(model, ids)
    z = scores(model.head, h)
    dz = cross_entropy_gradient(z, target)
    dh = mul(transposed(model.head.weights), dz)
    dmixed = pool_back(model.config.pooling, state.mixed, dh)
    if model.attention === nothing
        embed_back!(gradients[1], gradients[2], model.embedding, ids, dmixed)
        add_outer!(gradients[3], dz, h)
        gradients[4] .+= dz
    else
        dx, dattention = attend_back(model.attention, state.cache, dmixed)
        embed_back!(gradients[1], gradients[2], model.embedding, ids, dx)
        gradients[3] .+= dattention.query
        gradients[4] .+= dattention.key
        gradients[5] .+= dattention.value
        add_outer!(gradients[6], dz, h)
        gradients[7] .+= dz
    end
    cross_entropy(z, target)
end
