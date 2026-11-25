# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Inputs for the tests, drawn from a seeded generator so that a failure can
# be found again. `size` bounds how large an input may get: the length of a
# list, the number of vertices of a graph, the number of characters of a text.

"""
    random_list(rng, size; lo = -size, hi = size)

A list of whole numbers between `lo` and `hi`, of a length between 0 and
`size`. With a narrow range, repeats are common, which is what a sorting
test needs.
"""
function random_list(rng::SplitMix, size::Integer; lo::Integer = -size, hi::Integer = size)
    size >= 0 || throw(ArgumentError("a size cannot be negative"))
    lo <= hi || throw(ArgumentError("the range from $(lo) to $(hi) is empty"))
    Int[integer!(rng, lo:hi) for _ in 1:integer!(rng, 0:size)]
end

"A list as [`random_list`](@ref) draws it, sorted."
random_sorted_list(rng::SplitMix, size::Integer; lo::Integer = -size, hi::Integer = size) = sort!(random_list(rng, size; lo, hi))

"""
    random_array(rng, size; lo = -size, hi = size)

An array of exactly `size` whole numbers between `lo` and `hi`.
"""
function random_array(rng::SplitMix, size::Integer; lo::Integer = -size, hi::Integer = size)
    size >= 0 || throw(ArgumentError("a size cannot be negative"))
    Int[integer!(rng, lo:hi) for _ in 1:size]
end

"""
    random_graph(rng, size; density = 0.3)

A directed graph on the vertices `0` to `n - 1`, with `n` between 1 and
`size`: the number of vertices and the list of edges. Each ordered pair of
vertices is an edge with probability `density`.
"""
function random_graph(rng::SplitMix, size::Integer; density::Real = 0.3)
    size >= 1 || throw(ArgumentError("a graph has at least one vertex"))
    0 <= density <= 1 || throw(ArgumentError("a density is between 0 and 1"))
    n = integer!(rng, 1:size)
    edges = Tuple{Int,Int}[]
    for u in 0:(n - 1), v in 0:(n - 1)
        uniform!(rng) < density && push!(edges, (u, v))
    end
    (n, edges)
end

"The edges of a graph as a table from a vertex to the vertices its edges lead to."
function adjacency(n::Integer, edges::AbstractVector{<:Tuple})
    table = Dict{Int,Vector{Int}}(u => Int[] for u in 0:(n - 1))
    for (u, v) in edges
        push!(get!(table, u, Int[]), v)
    end
    table
end

"""
    random_string(rng, size; alphabet = "ab")

A text of between 0 and `size` characters of the alphabet. A small alphabet
makes it likely that a pattern occurs in a text, and more than once.
"""
function random_string(rng::SplitMix, size::Integer; alphabet::AbstractString = "ab")
    size >= 0 || throw(ArgumentError("a size cannot be negative"))
    isempty(alphabet) && throw(ArgumentError("the alphabet is empty"))
    letters = collect(alphabet)
    String(Char[letters[integer!(rng, 1:length(letters))] for _ in 1:integer!(rng, 0:size)])
end
