# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The flat index: exact nearest-neighbour search by scanning every vector.
# It is the reference that approximate indexes are measured against.

"One search result: the identifier of a stored vector and its score."
struct Hit
    id::String
    score::Float64
end

"""
    FlatIndex(dimension)

An exact index over unit vectors. Vectors are stored as the columns of one
matrix, so a query is a single matrix-vector product. Each vector is stored
under a string identifier, and row numbers never leak out of the index.
"""
mutable struct FlatIndex
    dimension::Int
    ids::Vector{String}
    positions::Dict{String,Int}
    data::Matrix{Float64}
    count::Int
    function FlatIndex(dimension::Integer)
        dimension > 0 || throw(ArgumentError("the dimension must be positive"))
        new(dimension, String[], Dict{String,Int}(), Matrix{Float64}(undef, dimension, 16), 0)
    end
end

Base.length(index::FlatIndex) = index.count
dimension(index::FlatIndex) = index.dimension

"""
    add!(index, id, vector)

Stores a vector under an identifier. Storing under an identifier that is
already present replaces its vector.
"""
function add!(index::FlatIndex, id::AbstractString, vector::AbstractVector{<:Real})
    length(vector) == index.dimension || throw(DimensionMismatch("expected a vector of dimension $(index.dimension)"))
    position = get(index.positions, id, 0)
    if position == 0
        index.count += 1
        position = index.count
        if position > size(index.data, 2)
            grown = Matrix{Float64}(undef, index.dimension, 2 * size(index.data, 2))
            grown[:, 1:(position - 1)] = index.data[:, 1:(position - 1)]
            index.data = grown
        end
        push!(index.ids, String(id))
        index.positions[String(id)] = position
    end
    index.data[:, position] = vector
    index
end

"""
    search(index, query; k = 10)

The `k` stored vectors with the largest dot product with `query`, best first.
Equal scores are ordered by identifier, so the result is deterministic.
"""
function search(index::FlatIndex, query::AbstractVector{<:Real}; k::Integer = 10)
    length(query) == index.dimension || throw(DimensionMismatch("expected a query of dimension $(index.dimension)"))
    index.count == 0 && return Hit[]
    scores = transpose(view(index.data, :, 1:index.count)) * query
    order = sortperm(collect(1:index.count); by = i -> (-scores[i], index.ids[i]))
    [Hit(index.ids[i], scores[i]) for i in order[1:min(k, index.count)]]
end
