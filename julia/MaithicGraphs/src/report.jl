# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The measures of a graph as one document.

const METRICS_SCHEMA = "maithic.intel.metrics/1"

"""
    metrics(g)

The measures of a code graph as a document (schema `maithic.intel.metrics/1`):
counts, the sizes of the weak components, and for every declaration of the
scanned files its degrees, its PageRank and its betweenness, highest rank
first. Ranks are rounded to nine places and betweenness to six, so that the
document is the same on every machine.
"""
function metrics(g::CodeGraph)
    rank = pagerank(g)
    between = betweenness(g)
    used = in_degree(g)
    uses = out_degree(g)
    order = sort!([i for i in 1:node_count(g) if g.internal[i]]; by = i -> (-rank[i], g.names[i]))
    Object(
        "schema" => METRICS_SCHEMA,
        "nodes" => node_count(g),
        "internal" => count(g.internal),
        "edges" => edge_count(g),
        "components" => Object("count" => length(component_sizes(g)), "sizes" => component_sizes(g)),
        "declarations" => [
            Object("name" => g.names[i], "kind" => g.kinds[i], "usedBy" => used[i], "uses" => uses[i],
                   "pagerank" => round(rank[i]; digits = 9), "betweenness" => round(between[i]; digits = 6))
            for i in order
        ],
    )
end
