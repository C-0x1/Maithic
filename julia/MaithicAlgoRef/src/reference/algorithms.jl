# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The reference implementations: what each algorithm should return, said in
# the shortest way Julia allows. None of this shares code with the Lean side.

"The algorithms that have a reference, by the names the executable uses."
reference_names() = ["insertion_sort", "merge_sort", "selection_sort", "quick_sort", "linear_search", "binary_search", "minimum", "maximum",
                     "dedup_sorted", "is_sorted", "same_counts", "fib", "prefix_sums", "range_sum", "gcd", "pow", "pow_mod", "find_sub", "path"]

"Zero-based index of the first element equal to `x`, or `nothing`."
first_index(x, xs) = (i = findfirst(==(x), xs); i === nothing ? nothing : i - 1)

"The Fibonacci numbers by the matrix identity, in big integers."
function fibonacci(n::Integer)
    a, b = big(0), big(1)
    for _ in 1:n
        a, b = b, a + b
    end
    a
end

"Zero-based index of the first occurrence of `pattern` in `text`, or `nothing`."
function first_occurrence(pattern::AbstractVector, text::AbstractVector)
    m, n = length(pattern), length(text)
    for start in 0:(n - m)
        all(text[start + k] == pattern[k] for k in 1:m) && return start
    end
    nothing
end

"The vertices that can be reached from `u`, by breadth-first search."
function reachable(n::Integer, edges::AbstractVector{<:Tuple}, u::Integer)
    next = adjacency(n, edges)
    seen = Set{Int}([u])
    queue = [u]
    while !isempty(queue)
        v = popfirst!(queue)
        for w in get(next, v, Int[])
            w in seen || (push!(seen, w); push!(queue, w))
        end
    end
    seen
end

"""
    reference(name, arguments...)

What the algorithm called `name` should return on the arguments. Lists come
back as vectors, positions as zero-based numbers or `nothing`, and a search
for a path as `true` or `false`: whether there is one. Which path is found
is the implementation's affair.
"""
function reference(name::AbstractString, arguments...)
    if name in ("insertion_sort", "merge_sort", "selection_sort", "quick_sort")
        sort(collect(arguments[1]))
    elseif name == "linear_search"
        first_index(arguments[1], arguments[2])
    elseif name == "binary_search"
        arguments[1] in arguments[2]
    elseif name == "minimum"
        isempty(arguments[1]) ? nothing : minimum(arguments[1])
    elseif name == "maximum"
        isempty(arguments[1]) ? nothing : maximum(arguments[1])
    elseif name == "dedup_sorted"
        unique(arguments[1])
    elseif name == "is_sorted"
        issorted(arguments[1])
    elseif name == "same_counts"
        sort(collect(arguments[1])) == sort(collect(arguments[2]))
    elseif name == "fib"
        fibonacci(arguments[1])
    elseif name == "prefix_sums"
        vcat(0, cumsum(arguments[1]))
    elseif name == "range_sum"
        i, j, xs = arguments
        sum(xs[(i + 1):j]; init = 0)
    elseif name == "gcd"
        gcd(arguments[1], arguments[2])
    elseif name == "pow"
        big(arguments[1])^arguments[2]
    elseif name == "pow_mod"
        b, n, m = arguments
        m == 1 ? big(0) : powermod(big(b), n, big(m))
    elseif name == "find_sub"
        first_occurrence(arguments[1], arguments[2])
    elseif name == "path"
        n, u, target, edges = arguments
        target in reachable(n, edges, u)
    else
        throw(ArgumentError("there is no reference for the algorithm \"$(name)\""))
    end
end
