# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reads a goal document (schema maithic.oracle.goal/1) from standard input and
# writes the outcome (schema maithic.oracle.outcome/1, with the members for
# Lean) to standard output. This is the program the `maithic` command runs to
# ask an oracle:
#
#     julia --startup-file=no --project=julia/MaithicOracle julia/MaithicOracle/bin/solve.jl < goal.json
#
# Exit status: 0 when an outcome was written, whatever it is; 2 when the goal
# could not be read, with the reason on standard error.

using MaithicOracle

function main()
    text = read(stdin, String)
    try
        println(answer(text))
        return 0
    catch e
        e isa ArgumentError || rethrow()
        println(stderr, "the goal could not be read: ", e.msg)
        return 2
    end
end

exit(main())
