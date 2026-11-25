# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Weak components: the pieces of the graph when the direction of edges is ignored.

"""
    weak_components(g)

The weak components of the graph: sets of declarations that are joined by uses
edges in either direction. Each component is a sorted list of nodes; the
largest comes first, and among components of one size the one with the
smallest node.
"""
function weak_components(g::CodeGraph)
    n = node_count(g)
    label = zeros(Int, n)
    components = Vector{Int}[]
    for start in 1:n
        label[start] != 0 && continue
        members = Int[]
        label[start] = length(components) + 1
        queue = [start]
        while !isempty(queue)
            v = pop!(queue)
            push!(members, v)
            for w in Iterators.flatten((g.uses[v], g.used_by[v]))
                if label[w] == 0
                    label[w] = label[start]
                    push!(queue, w)
                end
            end
        end
        push!(components, sort!(members))
    end
    sort!(components; by = c -> (-length(c), first(c)))
end

"The sizes of the weak components, largest first."
component_sizes(g::CodeGraph) = length.(weak_components(g))
