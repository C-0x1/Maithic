# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Degrees, PageRank and betweenness.

"How many declarations use each one."
in_degree(g::CodeGraph) = length.(g.used_by)

"How many declarations each one uses."
out_degree(g::CodeGraph) = length.(g.uses)

"""
    pagerank(g; damping = 0.85, tolerance = 1e-12, max_iterations = 500)

The PageRank of every declaration, with weight flowing from a declaration to
what it uses: a declaration ranks high when much rests on it, directly or
through others. A declaration that uses nothing passes its weight to all
declarations alike. The ranks are positive and sum to one.
"""
function pagerank(g::CodeGraph; damping::Float64 = 0.85, tolerance::Float64 = 1e-12, max_iterations::Int = 500)
    n = node_count(g)
    n == 0 && return Float64[]
    0.0 <= damping < 1.0 || throw(ArgumentError("damping must be at least 0 and below 1"))
    rank = fill(1.0 / n, n)
    for _ in 1:max_iterations
        dangling = sum(rank[i] for i in 1:n if isempty(g.uses[i]); init = 0.0)
        next = fill((1.0 - damping) / n + damping * dangling / n, n)
        for i in 1:n
            isempty(g.uses[i]) && continue
            share = damping * rank[i] / length(g.uses[i])
            for j in g.uses[i]
                next[j] += share
            end
        end
        change = sum(abs, next .- rank)
        rank = next
        change < tolerance && break
    end
    rank ./ sum(rank)
end

"""
    betweenness(g)

For every declaration, the number of shortest ways between two other
declarations that pass through it, where a pair with several shortest ways
counts each way by its share. Ways follow the uses edges in their direction.
Computed by Brandes' algorithm.
"""
function betweenness(g::CodeGraph)
    n = node_count(g)
    between = zeros(Float64, n)
    for source in 1:n
        stack = Int[]
        before = [Int[] for _ in 1:n]
        ways = zeros(Float64, n)
        ways[source] = 1.0
        distance = fill(-1, n)
        distance[source] = 0
        queue = [source]
        head = 1
        while head <= length(queue)
            v = queue[head]
            head += 1
            push!(stack, v)
            for w in g.uses[v]
                if distance[w] < 0
                    distance[w] = distance[v] + 1
                    push!(queue, w)
                end
                if distance[w] == distance[v] + 1
                    ways[w] += ways[v]
                    push!(before[w], v)
                end
            end
        end
        dependency = zeros(Float64, n)
        while !isempty(stack)
            w = pop!(stack)
            for v in before[w]
                dependency[v] += ways[v] / ways[w] * (1.0 + dependency[w])
            end
            w != source && (between[w] += dependency[w])
        end
    end
    between
end

"""
    top(g, scores, k = 10; internal_only = false)

The `k` declarations with the highest scores, as pairs of name and score; ties
go to the name that sorts first.
"""
function top(g::CodeGraph, scores::AbstractVector, k::Integer = 10; internal_only::Bool = false)
    length(scores) == node_count(g) || throw(ArgumentError("one score is needed for every node"))
    order = sort!([i for i in 1:node_count(g) if !internal_only || g.internal[i]]; by = i -> (-scores[i], g.names[i]))
    [g.names[i] => scores[i] for i in first(order, k)]
end
