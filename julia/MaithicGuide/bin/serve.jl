# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Serves a tactic model: requests as JSON lines on standard input, answers on
# standard output. `maithic` starts this for a search that is guided.
#
#     julia --project=julia/MaithicGuide julia/MaithicGuide/bin/serve.jl MODEL

using MaithicGuide

function main(args)
    length(args) == 1 || (println(stderr, "usage: serve.jl MODEL"); return 2)
    model = try
        load_model(args[1])
    catch e
        e isa ArgumentError || rethrow()
        println(stderr, "the model could not be loaded: ", e.msg)
        return 2
    end
    serve(model)
    return 0
end

exit(main(ARGS))
