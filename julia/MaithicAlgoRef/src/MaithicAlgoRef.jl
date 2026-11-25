# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicAlgoRef

Reference implementations of the algorithms that are verified in Lean, and
the differential tests that hold the compiled Lean code against them.

A proof in Lean says that a function meets its specification. Two things
it does not say: that the specification is the one that was meant, and that
the compiled program computes the function. This package looks from the
outside. Every algorithm of `Maithic.Algo` has a second implementation
here, written in another language and in another way, most of them one
line over Julia's standard library. Inputs are generated from a seed, the
executable `maithic-algo` is run on them, and each answer is compared with
what the reference says. An algorithm with several right answers, such as
a search for a path, is compared by what makes an answer right.

The same executable answers how many comparisons four of the algorithms
make. [`cost_table`](@ref) measures that on inputs of growing size and
lays each count beside the bound that Lean proves for it.
"""
module MaithicAlgoRef

using MaithicCore: SplitMix, integer!, uniform!, Object, document, write_json

export reference_names, reference
export random_list, random_sorted_list, random_array, random_graph, random_string, adjacency
export Case, make_case, cases, Mismatch, Report, run_differential, report_document, process_transport, reference_transport
export cost_names, proven_bound, cost_request, cost_table, cost_csv, within_bounds, line_transport, CostRow

const VERSION = v"0.1.0"

include("reference/algorithms.jl")
include("generate/inputs.jl")
include("differential/cases.jl")
include("differential/runner.jl")
include("differential/costs.jl")

end # module
