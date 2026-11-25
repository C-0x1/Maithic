# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The records of a dataset as typed values.

"A theorem with its proof."
struct TheoremRecord
    id::String
    name::String
    mod::String
    file::String
    line::Int
    statement::String
    proof::String
    steps::Int
    premises::Int
    complete::Bool
end

"One tactic of a proof as it ran."
struct StepRecord
    id::String
    theorem::String
    mod::String
    index::Int
    tactic::String
    goals_before::Vector{String}
    goals_after::Vector{String}
    depth::Int
    inner::Int
    line::Int
    column::Int
end

"One constant that the proof of a theorem refers to."
struct PremiseRecord
    id::String
    theorem::String
    mod::String
    premise::String
end

"A proof state and the tactic that was run on it."
struct PairRecord
    id::String
    theorem::String
    mod::String
    state::String
    tactic::String
    goals_before::Int
    goals_after::Int
end

"The four tables of a dataset, with its name and version."
struct Dataset
    name::String
    version::String
    theorems::Vector{TheoremRecord}
    steps::Vector{StepRecord}
    premises::Vector{PremiseRecord}
    pairs::Vector{PairRecord}
end

"The theorem a record belongs to, by module and name: what a split must keep together."
theorem_key(r::TheoremRecord) = (r.mod, r.name)
theorem_key(r::Union{StepRecord,PremiseRecord,PairRecord}) = (r.mod, r.theorem)

"What a record says, apart from where it comes from: equal for records that are the same example."
content(r::TheoremRecord) = (r.statement, r.proof)
content(r::StepRecord) = (r.tactic, r.goals_before, r.goals_after)
content(r::PremiseRecord) = (r.theorem, r.mod, r.premise)
content(r::PairRecord) = (r.state, r.tactic)
