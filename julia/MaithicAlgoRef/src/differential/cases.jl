# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Test cases: a request for the executable, and what makes an answer right.

"""
    Case

One request and its judgement. `request` is the line the implementation is
given. `sample` is a right answer, as the reference gives it. `accept` says
whether an answer is right: for most algorithms that is equality with the
sample, and for the ones with several right answers it is the property that
makes an answer right. `expected` says in words what is accepted.
"""
struct Case
    algorithm::String
    request::String
    sample::String
    expected::String
    accept::Function
end

exact_case(algorithm, request, answer) = Case(algorithm, request, answer, answer, ==(answer))

numbers(xs) = join(xs, " ")
position(i) = i === nothing ? "none" : string(i)

"A path by breadth-first search: one of the shortest, or `nothing`."
function some_path(n::Integer, edges, u::Integer, target::Integer)
    next = adjacency(n, edges)
    before = Dict{Int,Int}(u => u)
    queue = [u]
    while !isempty(queue)
        v = popfirst!(queue)
        if v == target
            path = [v]
            while path[1] != u
                pushfirst!(path, before[path[1]])
            end
            return path
        end
        for w in get(next, v, Int[])
            haskey(before, w) || (before[w] = v; push!(queue, w))
        end
    end
    nothing
end

"Whether a line of numbers is a path from `u` to `target` along the edges."
function is_path_answer(answer::AbstractString, edges, u::Integer, target::Integer)
    parts = split(answer)
    vertices = tryparse.(Int, parts)
    (isempty(vertices) || any(isnothing, vertices)) && return false
    vertices[1] == u && vertices[end] == target && all((vertices[k], vertices[k + 1]) in edges for k in 1:(length(vertices) - 1))
end

"""
    make_case(name, rng, size)

Draws one case for the algorithm called `name`. `size` bounds the input.
"""
function make_case(name::AbstractString, rng::SplitMix, size::Integer)
    size >= 1 || throw(ArgumentError("the size of a case is at least 1"))
    narrow = max(size ÷ 2, 1)
    if name in ("insertion_sort", "merge_sort", "selection_sort", "quick_sort")
        xs = random_list(rng, size; lo = -narrow, hi = narrow)
        exact_case(name, "$(name) $(numbers(xs))", numbers(reference(name, xs)))
    elseif name == "linear_search"
        xs = random_list(rng, size; lo = 0, hi = narrow)
        x = integer!(rng, 0:narrow)
        exact_case(name, "$(name) $(x) | $(numbers(xs))", position(reference(name, x, xs)))
    elseif name == "binary_search"
        xs = random_sorted_list(rng, size; lo = 0, hi = size)
        x = integer!(rng, 0:size)
        request = "$(name) $(x) | $(numbers(xs))"
        if reference(name, x, xs)
            holds(answer) = (i = tryparse(Int, answer); i !== nothing && 0 <= i < length(xs) && xs[i + 1] == x)
            Case(name, request, position(first_index(x, xs)), "a position that holds $(x)", holds)
        else
            exact_case(name, request, "none")
        end
    elseif name in ("minimum", "maximum")
        xs = random_list(rng, size)
        exact_case(name, "$(name) $(numbers(xs))", position(reference(name, xs)))
    elseif name == "dedup_sorted"
        xs = random_sorted_list(rng, size; lo = 0, hi = narrow)
        exact_case(name, "$(name) $(numbers(xs))", numbers(reference(name, xs)))
    elseif name == "is_sorted"
        xs = integer!(rng, 0:1) == 0 ? random_sorted_list(rng, size) : random_list(rng, size)
        exact_case(name, "$(name) $(numbers(xs))", string(reference(name, xs)))
    elseif name == "same_counts"
        xs = random_list(rng, size; lo = 0, hi = narrow)
        ys = integer!(rng, 0:1) == 0 ? reverse(xs) : random_list(rng, size; lo = 0, hi = narrow)
        exact_case(name, "$(name) $(numbers(xs)) | $(numbers(ys))", string(reference(name, xs, ys)))
    elseif name == "fib"
        n = integer!(rng, 0:(8 * size))
        exact_case(name, "$(name) $(n)", string(reference(name, n)))
    elseif name == "prefix_sums"
        xs = random_list(rng, size)
        exact_case(name, "$(name) $(numbers(xs))", numbers(reference(name, xs)))
    elseif name == "range_sum"
        xs = random_list(rng, size)
        j = integer!(rng, 0:length(xs))
        i = integer!(rng, 0:j)
        exact_case(name, "$(name) $(i) $(j) | $(numbers(xs))", string(reference(name, i, j, xs)))
    elseif name == "gcd"
        a, b = integer!(rng, 0:(size^3)), integer!(rng, 0:(size^3))
        exact_case(name, "$(name) $(a) $(b)", string(reference(name, a, b)))
    elseif name == "pow"
        b, n = integer!(rng, 0:size), integer!(rng, 0:(2 * size))
        exact_case(name, "$(name) $(b) $(n)", string(reference(name, b, n)))
    elseif name == "pow_mod"
        b, n, m = integer!(rng, 0:(size^2)), integer!(rng, 0:(size^2)), integer!(rng, 1:(size^2))
        exact_case(name, "$(name) $(b) $(n) $(m)", string(reference(name, b, n, m)))
    elseif name == "find_sub"
        text = Int.(collect(random_string(rng, size))) .- Int('a')
        pattern = Int.(collect(random_string(rng, min(size, 3)))) .- Int('a')
        exact_case(name, "$(name) $(numbers(pattern)) | $(numbers(text))", position(reference(name, pattern, text)))
    elseif name == "path"
        n, edges = random_graph(rng, min(size, 7))
        u, target = integer!(rng, 0:(n - 1)), integer!(rng, 0:(n - 1))
        request = "$(name) $(n) $(u) $(target) | $(numbers(Iterators.flatten(edges)))"
        if reference(name, n, u, target, edges)
            Case(name, request, numbers(some_path(n, edges, u, target)), "a path from $(u) to $(target)", answer -> is_path_answer(answer, edges, u, target))
        else
            exact_case(name, request, "none")
        end
    else
        throw(ArgumentError("there is no case for the algorithm \"$(name)\""))
    end
end

"""
    cases(; seed = 1, count = 190, size = 12, algorithms = reference_names())

`count` cases, the algorithms taken in turn. The same arguments give the
same cases.
"""
function cases(; seed::Integer = 1, count::Integer = 190, size::Integer = 12, algorithms = reference_names())
    isempty(algorithms) && throw(ArgumentError("no algorithm was named"))
    unknown = setdiff(algorithms, reference_names())
    isempty(unknown) || throw(ArgumentError("there is no reference for the algorithm \"$(first(unknown))\""))
    rng = SplitMix(UInt64(seed))
    [make_case(algorithms[mod1(k, length(algorithms))], rng, size) for k in 1:count]
end
