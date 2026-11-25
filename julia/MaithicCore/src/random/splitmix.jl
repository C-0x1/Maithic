# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# A pseudo-random generator that is the same everywhere.
#
# The streams of the generators in Julia's standard library may change between
# Julia versions. A search that reports "found with seed 7" must find the same
# thing next year, so seeded searches in Maithic use SplitMix64, whose stream
# is fixed by its definition.

"""
    SplitMix(seed)

The SplitMix64 generator. Equal seeds give equal streams on every platform
and every Julia version.
"""
mutable struct SplitMix
    state::UInt64
end

SplitMix(seed::Integer) = SplitMix(seed % UInt64)

"The next 64 bits of the stream."
function next_u64!(rng::SplitMix)
    rng.state += 0x9e3779b97f4a7c15
    z = rng.state
    z = (z ⊻ (z >> 30)) * 0xbf58476d1ce4e5b9
    z = (z ⊻ (z >> 27)) * 0x94d049bb133111eb
    z ⊻ (z >> 31)
end

"""
    uniform!(rng)
    uniform!(rng, lo, hi)

A `Float64` uniform on `[0, 1)`, or on `[lo, hi)`.
"""
uniform!(rng::SplitMix) = (next_u64!(rng) >> 11) * (1.0 / 9007199254740992.0)
uniform!(rng::SplitMix, lo::Real, hi::Real) = lo + (hi - lo) * uniform!(rng)

"""
    integer!(rng, range)

An integer uniform on `range`, without modulo bias.
"""
function integer!(rng::SplitMix, range::AbstractUnitRange{<:Integer})
    isempty(range) && throw(ArgumentError("cannot draw from an empty range"))
    span = UInt64(length(range))
    excess = (typemax(UInt64) % span + 1) % span
    while true
        x = next_u64!(rng)
        x <= typemax(UInt64) - excess && return first(range) + oftype(first(range), x % span)
    end
end

"""
    MaithicCore.shuffle!(rng, items)

Reorders `items` in place, uniformly at random, and returns them.
"""
function shuffle!(rng::SplitMix, items::AbstractVector)
    for i in length(items):-1:2
        j = integer!(rng, 1:i)
        items[i], items[j] = items[j], items[i]
    end
    items
end
