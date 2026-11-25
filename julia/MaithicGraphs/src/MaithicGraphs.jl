# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicGraphs

Measures on the graph of a Lean code base.

The `maithic intel scan` command writes a code graph: declarations, and which
declaration uses which. This package loads the declarations and the uses
edges of such a graph and computes what the shape of the graph says about the
code: how many declarations use each one, which ones everything rests on
(PageRank), which ones lie on the ways between others (betweenness), and how
the code falls into pieces that do not refer to each other (weak components).

Everything is exact or deterministic: the same graph gives the same numbers.
"""
module MaithicGraphs

using MaithicCore

export CodeGraph, load_graph, read_graph, node_count, edge_count
export in_degree, out_degree, pagerank, betweenness, top
export weak_components, component_sizes
export metrics, METRICS_SCHEMA

const VERSION = v"0.1.0"

include("load.jl")
include("centrality.jl")
include("components.jl")
include("report.jl")

end # module
