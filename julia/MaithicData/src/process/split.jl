# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Splitting a dataset by theorem, and checking that the parts do not overlap.

"""
    Split

An assignment of theorems to the parts `:train`, `:validation` and `:test`.
A theorem is named by its module and its name.
"""
struct Split
    seed::UInt64
    fractions::NTuple{3,Float64}
    parts::Dict{Tuple{String,String},Symbol}
end

const PARTS = (:train, :validation, :test)

"""
    random_split(theorems; seed = 0, validation = 0.1, test = 0.1)

Assigns every theorem to a part. The theorems are put in the order of their
keys and shuffled with a stream made from the seed, so the result depends on
the set of theorems and the seed and on nothing else: not on the order of the
records, and not on the machine. The test part gets `test` of the theorems,
rounded, the validation part `validation`, and training the rest; with three
theorems or more, no part asked for is left empty.
"""
function random_split(theorems::AbstractVector{TheoremRecord}; seed::Integer = 0, validation::Real = 0.1, test::Real = 0.1)
    0 <= validation && 0 <= test && validation + test < 1 ||
        throw(ArgumentError("the validation and test fractions must be at least 0 and sum to less than 1"))
    keys = sort!(unique!([theorem_key(t) for t in theorems]))
    rng = SplitMix(seed)
    for i in length(keys):-1:2
        j = integer!(rng, 1:i)
        keys[i], keys[j] = keys[j], keys[i]
    end
    n = length(keys)
    share(fraction) = fraction == 0 ? 0 : n >= 3 ? max(1, round(Int, fraction * n)) : round(Int, fraction * n)
    n_test = share(test)
    n_validation = min(share(validation), n - n_test)
    parts = Dict{Tuple{String,String},Symbol}()
    for (i, key) in enumerate(keys)
        parts[key] = i <= n_test ? :test : i <= n_test + n_validation ? :validation : :train
    end
    Split(seed % UInt64, (1.0 - validation - test, Float64(validation), Float64(test)), parts)
end

"The part a record belongs to: the part of its theorem."
function part_of(split::Split, record)
    key = theorem_key(record)
    haskey(split.parts, key) || throw(KeyError(key))
    split.parts[key]
end

"How many theorems each part holds."
split_counts(split::Split) = Dict(part => count(==(part), values(split.parts)) for part in PARTS)

"""
    split_manifest(split)

The split as a document: the seed, the fractions asked for, the number of
theorems in each part, and the theorems of each part in order.
"""
function split_manifest(split::Split)
    members(part) = [Object("module" => key[1], "name" => key[2]) for key in sort!([k for (k, p) in split.parts if p == part])]
    counts = split_counts(split)
    Object(
        "schema" => "maithic.corpus.split/1",
        "seed" => Int(split.seed),
        "fractions" => Object("train" => split.fractions[1], "validation" => split.fractions[2], "test" => split.fractions[3]),
        "theorems" => Object("train" => counts[:train], "validation" => counts[:validation], "test" => counts[:test]),
        "train" => members(:train), "validation" => members(:validation), "test" => members(:test),
    )
end

"A theorem that is in more than one part."
struct LeakageError <: Exception
    theorems::Vector{Tuple{String,String}}
end

Base.showerror(io::IO, e::LeakageError) =
    print(io, "LeakageError: ", length(e.theorems), " theorem(s) appear in more than one part, the first being ", join(first(e.theorems), "."))

"""
    leakage(parts...)

The theorems that occur in more than one of the given collections of records,
sorted. Each collection is one part of a split, of any kind of record.
"""
function leakage(parts::AbstractVector...)
    seen = Dict{Tuple{String,String},Int}()
    leaked = Set{Tuple{String,String}}()
    for (number, records) in enumerate(parts)
        for record in records
            key = theorem_key(record)
            previous = get!(seen, key, number)
            previous != number && push!(leaked, key)
        end
    end
    sort!(collect(leaked))
end

"""
    check_leakage(parts...)

Returns `true` when no theorem occurs in two parts, and throws a
[`LeakageError`](@ref) that lists the theorems otherwise.
"""
function check_leakage(parts::AbstractVector...)
    leaked = leakage(parts...)
    isempty(leaked) || throw(LeakageError(leaked))
    true
end
