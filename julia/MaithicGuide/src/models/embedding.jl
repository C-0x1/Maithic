# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# From token numbers to vectors.

"""
    Embedding(tokens, positions)

A vector for every token of the vocabulary and a vector for every position up
to the longest sequence the model reads; both are columns. The vector of a
token in a sequence is the sum of the two, so that the same token can mean
something else at the start of a goal than after the turnstile.
"""
struct Embedding
    tokens::Matrix{Float64}
    positions::Matrix{Float64}
end

"An embedding with small random weights from a stream."
Embedding(rng::SplitMix, vocabulary::Integer, dim::Integer, max_length::Integer; deviation::Real = 0.1) =
    Embedding(normal_matrix(rng, dim, vocabulary, deviation), normal_matrix(rng, dim, max_length, deviation))

dimension(e::Embedding) = size(e.tokens, 1)
max_length(e::Embedding) = size(e.positions, 2)

"""
    clip(embedding, ids)

The token numbers a model reads of a sequence: the first as many as it has
positions for, and a single padding token for an empty sequence.
"""
clip(e::Embedding, ids::AbstractVector{<:Integer}) = isempty(ids) ? [1] : collect(ids[1:min(length(ids), max_length(e))])

"""
    embed(embedding, ids)

The vectors of a sequence of token numbers, one column for each token.
"""
function embed(e::Embedding, ids::AbstractVector{<:Integer})
    ids = clip(e, ids)
    x = Matrix{Float64}(undef, dimension(e), length(ids))
    for (position, id) in enumerate(ids)
        1 <= id <= size(e.tokens, 2) || throw(ArgumentError("$(id) is not the number of a token; the embedding has $(size(e.tokens, 2))"))
        @views x[:, position] .= e.tokens[:, id] .+ e.positions[:, position]
    end
    x
end

# The gradient of a loss with respect to the two tables, from its gradient
# with respect to the vectors of one sequence. Added to what is there.
function embed_back!(dtokens::Matrix{Float64}, dpositions::Matrix{Float64}, e::Embedding, ids::AbstractVector{<:Integer}, dx::Matrix{Float64})
    for (position, id) in enumerate(clip(e, ids))
        @views dtokens[:, id] .+= dx[:, position]
        @views dpositions[:, position] .+= dx[:, position]
    end
    nothing
end
