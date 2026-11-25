# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicEmbed

Embeddings and vector search for mathematical statements.

The package turns declarations and queries into unit vectors and finds the
nearest ones. The embedder that ships here needs no model and no download: it
hashes the character and token n-grams of a text into a fixed number of
dimensions. It is the baseline that any other embedder has to beat. The
`maithic search` command ranks declarations by their words and names and does
not call this package; the package is the vector side of the search pillar,
for use from Julia.
"""
module MaithicEmbed

export Embedder, HashedNgramEmbedder, embed, dimension, tokenize, Mode, Query, Document
export FlatIndex, add!, search, Hit, vector_json, parse_vector_json, vector_bytes, parse_vector_bytes, cosine

const VERSION = v"0.1.0"

include("embed/tokenize.jl")
include("embed/hashed.jl")
include("embed/serialize.jl")
include("index/flat.jl")

end # module
