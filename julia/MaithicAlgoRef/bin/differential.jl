# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Holds the compiled Lean algorithms against the reference implementations
# and prints a report (schema maithic.algolab.differential/1).
#
#     julia --project=julia/MaithicAlgoRef julia/MaithicAlgoRef/bin/differential.jl EXECUTABLE [SEED [COUNT [SIZE [ALGORITHM...]]]]
#
# EXECUTABLE is the program `maithic-algo` that `lake build` makes. The exit
# status is 0 when every answer is accepted, 1 when one is not, and 2 when
# the run could not be made.

using MaithicAlgoRef
using MaithicCore

function main(args)
    if isempty(args)
        println(stderr, "usage: differential.jl EXECUTABLE [SEED [COUNT [SIZE [ALGORITHM...]]]]")
        return 2
    end
    try
        number(k, default) = length(args) >= k ? parse(Int, args[k]) : default
        algorithms = length(args) > 4 ? String.(args[5:end]) : reference_names()
        report = run_differential(process_transport(Cmd([args[1]])); seed = number(2, 1), count = number(3, 190), size = number(4, 12), algorithms)
        println(write_json(report_document(report; implementation = basename(args[1])); pretty = true))
        return isempty(report.mismatches) ? 0 : 1
    catch e
        e isa Union{ArgumentError,Base.IOError,ProcessFailedException} || rethrow()
        println(stderr, sprint(showerror, e))
        return 2
    end
end

exit(main(ARGS))
