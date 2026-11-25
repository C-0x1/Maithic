# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Loading shards into typed tables.

"A dataset that cannot be loaded: a missing file, a wrong hash, a record that is not what its shard says."
struct DatasetError <: Exception
    message::String
end

Base.showerror(io::IO, e::DatasetError) = print(io, "DatasetError: ", e.message)

const SCHEMAS = Dict("theorem" => "maithic.corpus.theorem/1", "step" => "maithic.corpus.step/1",
                     "premise" => "maithic.corpus.premise/1", "pair" => "maithic.corpus.pair/1")

texts(values) = String[String(v) for v in values]

record_of(::Val{:theorem}, o) = TheoremRecord(o["id"], o["name"], o["module"], o["file"], o["line"], o["statement"], o["proof"],
                                              o["steps"], o["premises"], o["complete"])
record_of(::Val{:step}, o) = StepRecord(o["id"], o["theorem"], o["module"], o["index"], o["tactic"], texts(o["goals_before"]),
                                        texts(o["goals_after"]), o["depth"], o["inner"], o["line"], o["column"])
record_of(::Val{:premise}, o) = PremiseRecord(o["id"], o["theorem"], o["module"], o["premise"])
record_of(::Val{:pair}, o) = PairRecord(o["id"], o["theorem"], o["module"], o["state"], o["tactic"], o["goals_before"], o["goals_after"])

"""
    load_records(kind, text)

The records of one shard: `text` holds one JSON object on each line, each
naming the schema of `kind` (`"theorem"`, `"step"`, `"premise"` or `"pair"`).
"""
function load_records(kind::AbstractString, text::AbstractString)
    haskey(SCHEMAS, kind) || throw(DatasetError("there is no kind of record called '$kind'"))
    records = []
    for (number, line) in enumerate(split(text, '\n'))
        isempty(strip(line)) && continue
        object = try
            read_json(line)
        catch e
            e isa JSONError || rethrow()
            throw(DatasetError("line $number of a $kind shard is not JSON"))
        end
        object isa Object && get(object, "schema", "") == SCHEMAS[kind] ||
            throw(DatasetError("line $number of a $kind shard is not a $kind record"))
        record = try
            record_of(Val(Symbol(kind)), object)
        catch e
            e isa Union{KeyError,MethodError} || rethrow()
            throw(DatasetError("line $number of a $kind shard lacks a member or has one of the wrong type"))
        end
        push!(records, record)
    end
    records
end

"""
    load_dataset(dir)

Loads a dataset from its directory: the manifest is read, and every shard is
loaded after its SHA-256 has been compared with the one the manifest states.
"""
function load_dataset(dir::AbstractString)
    path = joinpath(dir, "manifest.json")
    isfile(path) || throw(DatasetError("$path is not there"))
    manifest = read_json(read(path, String))
    manifest isa Object && get(manifest, "schema", "") == "maithic.corpus.manifest/1" ||
        throw(DatasetError("$path is not a dataset manifest"))
    tables = Dict(kind => [] for kind in keys(SCHEMAS))
    for shard in manifest["shards"]
        file = joinpath(dir, shard["file"])
        isfile(file) || throw(DatasetError("the shard $(shard["file"]) is missing"))
        text = read(file, String)
        bytes2hex(sha256(text)) == shard["sha256"] ||
            throw(DatasetError("the shard $(shard["file"]) does not have the hash the manifest states"))
        records = load_records(shard["kind"], text)
        length(records) == shard["records"] ||
            throw(DatasetError("the shard $(shard["file"]) holds $(length(records)) records, and the manifest states $(shard["records"])"))
        append!(tables[shard["kind"]], records)
    end
    Dataset(manifest["name"], manifest["version"], TheoremRecord[tables["theorem"]...], StepRecord[tables["step"]...],
            PremiseRecord[tables["premise"]...], PairRecord[tables["pair"]...])
end
