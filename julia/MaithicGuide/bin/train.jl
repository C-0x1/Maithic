# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Trains a tactic model on a dataset written by `maithic corpus harvest` and
# writes the model and the record of the run.
#
#     julia --project=julia/MaithicGuide julia/MaithicGuide/bin/train.jl DATASET OUT [options]
#
# Options, each followed by a value: --name, --seed, --epochs, --batch, --dim,
# --max-length, --pooling (mean or first), --mixer (none or attention),
# --rate, --weight-decay, --k.

using MaithicGuide
using MaithicCore
using MaithicData

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

function main(args)
    length(args) >= 2 || (println(stderr, "usage: train.jl DATASET OUT [--name NAME] [--seed N] [--epochs N] [--dim N] [--pooling mean|first] [--mixer none|attention]"); return 2)
    dataset, out = args[1], args[2]
    try
        found = options(args[3:end])
        unknown = setdiff(keys(found), ["name", "seed", "epochs", "batch", "dim", "max-length", "pooling", "mixer", "rate", "weight-decay", "k"])
        isempty(unknown) || throw(ArgumentError("there is no option --$(first(sort!(collect(unknown))))"))
        seed = number(found, "seed", 0)
        config = TrainConfig(model = ModelConfig(dim = number(found, "dim", 32), max_length = number(found, "max-length", 128),
                                                 pooling = Symbol(get(found, "pooling", "mean")), mixer = Symbol(get(found, "mixer", "none"))),
                             optimizer = Adam(rate = number(found, "rate", 0.01), weight_decay = number(found, "weight-decay", 1e-4)),
                             epochs = number(found, "epochs", 30), batch = number(found, "batch", 16), k = number(found, "k", 5), seed = seed)
        examples = load_examples(dataset; seed = seed)
        result = train(examples.train, examples.validation; config = config)
        name = get(found, "name", string(config.model.mixer === :attention ? "attention-" : "", config.model.pooling, "-", config.model.dim))
        record = run_document(name, config, examples, result)
        mkpath(out)
        save_model(joinpath(out, "model.json"), result.model)
        write(joinpath(out, "run.json"), write_json(record; pretty = true), "\n")
        test, base = record["test"], record["baseline"]
        shown(x) = x === nothing ? "-" : string(round(100 * x; digits = 1), "%")
        println("trained $(name) on $(examples.dataset): $(length(examples.train)) training, $(length(examples.validation)) validation and $(length(examples.test)) test examples, ",
                "$(record["classes"]) templates, $(record["parameters"]) weights")
        println("best epoch $(result.best_epoch) of $(config.epochs); on the test part: top-1 $(shown(test["top1"])), top-$(config.k) $(shown(test["topk"]))")
        println("  always answering with the most frequent templates gives $(shown(base["top1"])) and $(shown(base["topk"]))")
        println("wrote $(joinpath(out, "model.json")) and $(joinpath(out, "run.json"))")
        return 0
    catch e
        e isa Union{ArgumentError,DatasetError} || rethrow()
        println(stderr, "training failed: ", e isa ArgumentError ? e.msg : e.message)
        return 2
    end
end

exit(main(ARGS))
