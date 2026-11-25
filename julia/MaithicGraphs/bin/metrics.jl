# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reads a code graph (schema maithic.intel.graph/1) from the file named as the
# first argument, or from standard input, and writes its measures (schema
# maithic.intel.metrics/1) to standard output.
#
#     julia --project=julia/MaithicGraphs julia/MaithicGraphs/bin/metrics.jl graph.json > metrics.json

using MaithicGraphs
using MaithicCore

function main(args)
    text = isempty(args) ? read(stdin, String) : read(args[1], String)
    try
        println(write_json(metrics(load_graph(text)); pretty = true))
        return 0
    catch e
        e isa ArgumentError || rethrow()
        println(stderr, "the graph could not be read: ", e.msg)
        return 2
    end
end

exit(main(ARGS))
