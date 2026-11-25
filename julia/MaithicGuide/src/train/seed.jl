# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# One seed for everything that is random in a training run.
#
# A run draws random numbers for several purposes: the first weights, the
# order of the examples in each epoch, the split of the data. If they all drew
# from one stream, a change in one place would shift the numbers of every
# other: one more weight, and the examples come in another order. Each purpose
# has a stream of its own instead, made from the seed and the name of the
# purpose, so that the seed alone decides the run and the purposes do not
# disturb each other.

# FNV-1a over the bytes of a name. Julia's own `hash` may change between
# versions; this one is fixed by its definition.
function name_hash(name::AbstractString)
    h = 0xcbf29ce484222325
    for byte in codeunits(name)
        h = (h ⊻ byte) * 0x00000100000001b3
    end
    h
end

"""
    stream(seed, purpose)

The random stream of a seed for one purpose, such as `"weights"` or
`"order"`. Equal seeds and purposes give equal streams on every machine and
every version of Julia; different purposes give streams that have nothing to
do with each other.
"""
function stream(seed::Integer, purpose::AbstractString)
    mixer = SplitMix((seed % UInt64) ⊻ name_hash(purpose))
    SplitMix(next_u64!(mixer))
end

"""
    normal!(rng)

A number from the normal distribution with mean 0 and deviation 1, by the
method of Box and Muller.
"""
function normal!(rng::SplitMix)
    u = uniform!(rng)
    while u <= 0.0
        u = uniform!(rng)
    end
    sqrt(-2.0 * log(u)) * cos(2.0 * pi * uniform!(rng))
end

"A matrix of normal numbers of the given deviation, filled column by column."
function normal_matrix(rng::SplitMix, rows::Integer, columns::Integer, deviation::Real)
    m = Matrix{Float64}(undef, rows, columns)
    for j in 1:columns, i in 1:rows
        m[i, j] = deviation * normal!(rng)
    end
    m
end
