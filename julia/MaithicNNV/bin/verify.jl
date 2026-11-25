# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Decides a property of a network and prints what it found (schema
# maithic.nnv.verdict/1) to standard output.
#
#     julia --project=julia/MaithicNNV julia/MaithicNNV/bin/verify.jl MODEL PROPERTY [MAX_CORNERS]
#
# MODEL is a network (schema maithic.nnv.network/1) and PROPERTY an input box
# with a claimed output box (schema maithic.nnv.property/1). The verdict is a
# claim: `maithic nnv verify` runs this program and then has the checkers in
# Lean look at the certificate or at the counterexample.

using MaithicNNV
using MaithicCore

function main(args)
    if !(2 <= length(args) <= 3)
        println(stderr, "usage: verify.jl MODEL PROPERTY [MAX_CORNERS]")
        return 2
    end
    try
        net = network_from_document(read_json(read(args[1], String)))
        input, claimed = property_from_document(read_json(read(args[2], String)))
        max_corners = length(args) == 3 ? parse(Int, args[3]) : 1024
        println(write_json(verdict_document(net, input, claimed; max_corners); pretty = true))
        return 0
    catch e
        e isa Union{ArgumentError,JSONError,SystemError} || rethrow()
        println(stderr, e isa SystemError ? "a file could not be read: $(e.prefix)" : sprint(showerror, e))
        return 2
    end
end

exit(main(ARGS))
