# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Trains every shape of tactic model on several seeds and writes the record
# of each run, for a comparison of the shapes: `maithicstats::read_guide_runs`
# reads the records.
#
#     julia --project=julia/MaithicGuide julia/MaithicGuide/bin/compare.jl DATASET OUT [options]
#
# Options, each followed by a value: --seeds (a list such as 1,2,3), --dim,
# --max-length, --epochs, --batch, --k. A seed decides the split of the data
# and the first weights, so the shapes meet on the same split seed by seed.

using MaithicGuide
using MaithicCore
using MaithicData

const SHAPES = [(:none, :mean), (:none, :first), (:attention, :mean), (:attention, :first)]

function options(args)
    found = Dict{String,String}()
    i = 1
    while i <= length(args)
        startswith(args[i], "--") && i < length(args) || throw(ArgumentError("expected an option with a value, and found $(args[i])"))
        found[args[i][3:end]] = args[i + 1]
        i += 2
    end
    found
end

function number(found, name, default)
    haskey(found, name) || return default
    value = tryparse(typeof(default), found[name])
    value === nothing ? throw(ArgumentError("--$(name) needs a number, and got $(found[name])")) : value
end

function seeds_of(text)
    seeds = [tryparse(Int, strip(part)) for part in split(text, ',')]
    any(isnothing, seeds) || isempty(seeds) ? throw(ArgumentError("--seeds needs whole numbers with commas between them, and got $(text)")) : Int[s for s in seeds]
end

function main(args)
    length(args) >= 2 || (println(stderr, "usage: compare.jl DATASET OUT [--seeds 1,2,3] [--dim N] [--epochs N]"); return 2)
    dataset, out = args[1], args[2]
    try
        found = options(args[3:end])
        unknown = setdiff(keys(found), ["seeds", "dim", "max-length", "epochs", "batch", "k"])
        isempty(unknown) || throw(ArgumentError("there is no option --$(first(sort!(collect(unknown))))"))
        seeds = seeds_of(get(found, "seeds", "1,2,3"))
        dim = number(found, "dim", 32)
        data = load_dataset(dataset)
        mkpath(out)
        shown(x) = x === nothing ? "-" : string(round(100 * x; digits = 1), "%")
        for seed in seeds
            examples = load_examples(data; seed = seed)
            for (mixer, pooling) in SHAPES
                config = TrainConfig(model = ModelConfig(dim = dim, max_length = number(found, "max-length", 128), pooling = pooling, mixer = mixer),
                                     epochs = number(found, "epochs", 30), batch = number(found, "batch", 16), k = number(found, "k", 5), seed = seed)
                name = string(mixer === :attention ? "attention-" : "", pooling, "-", dim)
                record = run_document(name, config, examples, train(examples.train, examples.validation; config = config))
                write(joinpath(out, "$(name)-seed-$(seed).json"), write_json(record; pretty = true), "\n")
                println(rpad(name, 20), " seed ", seed, ": top-1 ", shown(record["test"]["top1"]), ", top-", config.k, " ", shown(record["test"]["topk"]),
                        " on ", record["test"]["examples"], " examples (baseline ", shown(record["baseline"]["top1"]), ", ", shown(record["baseline"]["topk"]), ")")
            end
        end
        return 0
    catch e
        e isa Union{ArgumentError,DatasetError} || rethrow()
        println(stderr, "the comparison failed: ", e isa ArgumentError ? e.msg : e.message)
        return 2
    end
end

exit(main(ARGS))
