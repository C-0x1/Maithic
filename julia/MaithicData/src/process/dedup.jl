# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Removing records that say the same thing twice.

"""
    deduplicate(records; by = content)

The records without repeats: of several records for which `by` gives the same
value, the first is kept. The order of the rest does not change. By default
two records are the same when they are the same example, wherever they come
from: two pairs with the same state and tactic, two theorems with the same
statement and proof.
"""
function deduplicate(records::AbstractVector; by = content)
    seen = Set{Any}()
    kept = similar(records, 0)
    for record in records
        key = by(record)
        key in seen && continue
        push!(seen, key)
        push!(kept, record)
    end
    kept
end

"""
    duplicates(records; by = content)

The groups of records that `deduplicate` would merge: every group holds the
positions of two or more records with the same value of `by`, in order.
"""
function duplicates(records::AbstractVector; by = content)
    groups = Dict{Any,Vector{Int}}()
    order = Any[]
    for (i, record) in enumerate(records)
        key = by(record)
        haskey(groups, key) || push!(order, key)
        push!(get!(groups, key, Int[]), i)
    end
    [groups[key] for key in order if length(groups[key]) > 1]
end
