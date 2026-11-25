# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Loading a code graph into adjacency lists.

"""
    CodeGraph

The declarations of a code graph and the uses edges between them, as adjacency
lists. Node `i` is `names[i]`; `uses[i]` lists the nodes that declaration `i`
uses, and `used_by[i]` the nodes that use it, both sorted and without repeats.
"""
struct CodeGraph
    names::Vector{String}
    kinds::Vector{String}
    internal::Vector{Bool}
    index::Dict{String,Int}
    uses::Vector{Vector{Int}}
    used_by::Vector{Vector{Int}}
end

node_count(g::CodeGraph) = length(g.names)
edge_count(g::CodeGraph) = sum(length, g.uses; init = 0)

"""
    CodeGraph(names, edges; kinds, internal)

A graph from names and pairs `user => used` of names. An edge that names a node
which is not there is an error; an edge from a node to itself is left out.
"""
function CodeGraph(names::Vector{String}, edges; kinds::Vector{String} = fill("", length(names)),
                   internal::Vector{Bool} = fill(true, length(names)))
    length(unique(names)) == length(names) || throw(ArgumentError("a node is named twice"))
    index = Dict(name => i for (i, name) in enumerate(names))
    uses = [Int[] for _ in names]
    used_by = [Int[] for _ in names]
    for (user, used) in edges
        haskey(index, user) || throw(ArgumentError("an edge starts at a node that is not there: $user"))
        haskey(index, used) || throw(ArgumentError("an edge leads to a node that is not there: $used"))
        i, j = index[user], index[used]
        i == j && continue
        push!(uses[i], j)
        push!(used_by[j], i)
    end
    foreach(list -> unique!(sort!(list)), uses)
    foreach(list -> unique!(sort!(list)), used_by)
    CodeGraph(names, kinds, internal, index, uses, used_by)
end

"""
    read_graph(path; internal_only = false)

The graph in a file, as `maithic intel scan` writes it; see [`load_graph`](@ref).
"""
function read_graph(path::AbstractString; internal_only::Bool = false)
    isfile(path) || throw(ArgumentError("no code graph at $path; `maithic intel scan PATH` writes one"))
    load_graph(read(path, String); internal_only = internal_only)
end

"""
    load_graph(text; internal_only = false)

The declarations and uses edges of a code graph document (schema
`maithic.intel.graph/1`). With `internal_only`, declarations from outside the
scanned files are left out, with the edges that touch them.
"""
function load_graph(text::AbstractString; internal_only::Bool = false)
    doc = read_json(text)
    doc isa Object || throw(ArgumentError("a code graph is a JSON object"))
    get(doc, "schema", "") == "maithic.intel.graph/1" ||
        throw(ArgumentError("the document is not a code graph (maithic.intel.graph/1)"))
    names = String[]
    kinds = String[]
    internal = Bool[]
    ids = Dict{String,String}()
    for node in doc["nodes"]
        node["kind"] == "declaration" || continue
        inside = get(node, "internal", true)
        internal_only && !inside && continue
        push!(names, node["name"])
        push!(kinds, get(node, "declKind", ""))
        push!(internal, inside)
        ids[node["id"]] = node["name"]
    end
    edges = Pair{String,String}[]
    for edge in doc["edges"]
        edge["kind"] == "uses" || continue
        haskey(ids, edge["source"]) && haskey(ids, edge["target"]) || continue
        push!(edges, ids[edge["source"]] => ids[edge["target"]])
    end
    CodeGraph(names, edges; kinds, internal)
end
