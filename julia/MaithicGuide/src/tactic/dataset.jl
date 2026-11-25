# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Examples for the tactic classifier, from a dataset of proofs.

"""
    Example(theorem, state, tactic, closes)

One step of a proof: the goal the tactic was run on, the tactic, and whether
fewer goals stood after it than before. `theorem` is the key of the theorem
the step belongs to, which is what a split goes by.
"""
struct Example
    theorem::String
    state::String
    tactic::String
    closes::Bool
end

Example(pair::PairRecord) = Example(string(pair.mod, "\0", pair.theorem), main_goal(pair.state), pair.tactic, pair.goals_after < pair.goals_before)

"""
    Examples

The examples of a dataset in three parts, with what was read: the name and
version of the dataset, the number of pairs, and the number of theorems the
pairs come from.
"""
struct Examples
    train::Vector{Example}
    validation::Vector{Example}
    test::Vector{Example}
    dataset::String
    version::String
    pairs::Int
    theorems::Int
end

"""
    load_examples(dataset; seed = 0, validation = 0.1, test = 0.2)
    load_examples(dir; seed = 0, validation = 0.1, test = 0.2)

The state-and-tactic pairs of a dataset as examples, split into a training,
a validation and a test part. A pair that occurs twice is taken once. The
split is by theorem: every step of a proof is in the same part, so a model is
never asked about a state from a proof it was trained on. Only theorems with
a pair are split, so a part that was asked for is not left without examples
by theorems that were proved without tactics. The split is decided by the
seed and those theorems, and by nothing else.
"""
function load_examples(dataset::Dataset; seed::Integer = 0, validation::Real = 0.1, test::Real = 0.2)
    pairs = deduplicate(dataset.pairs)
    with_pairs = Set((pair.mod, pair.theorem) for pair in pairs)
    theorems = [t for t in dataset.theorems if (t.mod, t.name) in with_pairs]
    length(unique!([(t.mod, t.name) for t in theorems])) == length(with_pairs) ||
        throw(ArgumentError("the dataset has pairs of a theorem it has no record of"))
    split = random_split(theorems; seed = seed, validation = validation, test = test)
    parts = Dict(:train => Example[], :validation => Example[], :test => Example[])
    for pair in pairs
        push!(parts[part_of(split, pair)], Example(pair))
    end
    Examples(parts[:train], parts[:validation], parts[:test], dataset.name, dataset.version, length(pairs), length(with_pairs))
end

load_examples(dir::AbstractString; kwargs...) = load_examples(load_dataset(dir); kwargs...)
