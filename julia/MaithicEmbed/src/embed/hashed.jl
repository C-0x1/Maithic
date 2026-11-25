# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The embedder interface and the hashed n-gram embedder.

"""
    Embedder

An embedder turns texts into unit vectors. Implement `embed(e, text, mode)` and
`dimension(e)`; `embed(e, texts, mode)` then works on a list.
"""
abstract type Embedder end

"Whether a text is a search query or a document to be found."
@enum Mode Query Document

"""
    HashedNgramEmbedder(; dimension = 512, char_ngrams = 3:4, token_ngrams = 1:2, seed = 0)

A dependency-free embedder. Every token n-gram and every character n-gram of a
text is hashed to one of `dimension` coordinates with a sign, the weighted
counts are summed and the vector is scaled to unit length. Two texts that share
vocabulary and word order land close together.

The hash is FNV-1a, defined here, so the result does not depend on the Julia
version or the machine.
"""
struct HashedNgramEmbedder <: Embedder
    dimension::Int
    char_ngrams::UnitRange{Int}
    token_ngrams::UnitRange{Int}
    seed::UInt64
    function HashedNgramEmbedder(; dimension::Integer = 512, char_ngrams = 3:4, token_ngrams = 1:2, seed::Integer = 0)
        dimension > 0 || throw(ArgumentError("the dimension must be positive"))
        new(dimension, char_ngrams, token_ngrams, UInt64(seed))
    end
end

dimension(e::HashedNgramEmbedder) = e.dimension

"64-bit FNV-1a over the bytes of a string, with a seed mixed in."
function fnv1a(text::AbstractString, seed::UInt64)
    h = 0xcbf29ce484222325 ⊻ seed
    for byte in codeunits(text)
        h = (h ⊻ UInt64(byte)) * 0x100000001b3
    end
    h
end

function add_feature!(v::Vector{Float64}, e::HashedNgramEmbedder, feature::AbstractString, weight::Float64)
    h = fnv1a(feature, e.seed)
    index = Int(h % UInt64(e.dimension)) + 1
    sign = (h >> 63) == 0 ? 1.0 : -1.0
    v[index] += sign * weight
end

"""
    embed(embedder, text, mode = Document)

The unit vector of one text. A text with no features gives the zero vector.
Queries and documents are preprocessed differently: a query is usually a
fragment, so its character n-grams count for more.
"""
function embed(e::HashedNgramEmbedder, text::AbstractString, mode::Mode = Document)
    v = zeros(Float64, e.dimension)
    tokens = tokenize(text)
    for n in e.token_ngrams, i in 1:(length(tokens) - n + 1)
        add_feature!(v, e, string("t", n, ":", join(view(tokens, i:(i + n - 1)), " ")), 1.0)
    end
    char_weight = mode == Query ? 0.75 : 0.5
    joined = collect(join(tokens, " "))
    for n in e.char_ngrams, i in 1:(length(joined) - n + 1)
        add_feature!(v, e, string("c", n, ":", String(joined[i:(i + n - 1)])), char_weight)
    end
    norm = sqrt(sum(abs2, v))
    norm == 0 ? v : v ./ norm
end

"Embeds a list of texts; the result has one vector per text."
embed(e::Embedder, texts::AbstractVector{<:AbstractString}, mode::Mode = Document) = [embed(e, t, mode) for t in texts]

"The cosine similarity of two vectors; zero when either has no length."
function cosine(a::AbstractVector{<:Real}, b::AbstractVector{<:Real})
    length(a) == length(b) || throw(DimensionMismatch("vectors of different dimension"))
    na, nb = sqrt(sum(abs2, a)), sqrt(sum(abs2, b))
    (na == 0 || nb == 0) ? 0.0 : sum(a[i] * b[i] for i in eachindex(a)) / (na * nb)
end
