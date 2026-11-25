# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Measures how many comparisons the compiled Lean algorithms make on inputs
# of growing size, and prints the table as CSV with the proved bounds.
#
#     julia --project=julia/MaithicAlgoRef julia/MaithicAlgoRef/bin/costs.jl EXECUTABLE [SIZE...]
#
# EXECUTABLE is the program `maithic-algo` that `lake build` makes. The exit
# status is 0 when every count is within its bound, 1 when one is not, and 2
# when the measurement could not be made.

using MaithicAlgoRef

function main(args)
    if isempty(args)
        println(stderr, "usage: costs.jl EXECUTABLE [SIZE...]")
        return 2
    end
    try
        sizes = length(args) > 1 ? parse.(Int, args[2:end]) : MaithicAlgoRef.COST_SIZES
        rows = cost_table(line_transport(Cmd([args[1]])); sizes)
        print(cost_csv(rows))
        return within_bounds(rows) ? 0 : 1
    catch e
        e isa Union{ArgumentError,Base.IOError,ProcessFailedException} || rethrow()
        println(stderr, sprint(showerror, e))
        return 2
    end
end

exit(main(ARGS))
