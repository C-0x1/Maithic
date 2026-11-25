# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicData

Processing of the datasets that `maithic corpus harvest` writes.

A dataset is a manifest and shards of JSON lines. This package loads the
records into typed tables, removes records that say the same thing twice,
splits a dataset into training, validation and test parts by theorem, and
checks that no theorem ended up in two parts. Splits are made from a seed with
a fixed random stream, so the same dataset and seed give the same split on
every machine.
"""
module MaithicData

using MaithicCore
using SHA

export TheoremRecord, StepRecord, PremiseRecord, PairRecord, Dataset
export load_dataset, load_records, DatasetError
export deduplicate, duplicates
export Split, random_split, part_of, split_counts, split_manifest
export leakage, check_leakage, LeakageError

const VERSION = v"0.1.0"

include("process/records.jl")
include("process/load.jl")
include("process/dedup.jl")
include("process/split.jl")

end # module
