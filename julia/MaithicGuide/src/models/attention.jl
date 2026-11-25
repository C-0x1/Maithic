# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# A layer in which every token looks at every other.

"""
    Attention(query, key, value)

One head of self-attention with a residual connection. For the vectors `x`
of a sequence, as columns, the layer computes for each position a weighting
of all positions, from how well its query fits their keys, and adds the
values weighted so:

    scores  = (query * x)' * (key * x) / sqrt(dim)
    weights = softmax of each row of scores
    y       = x + (value * x) * weights'

After it, the vector at a position knows about the whole sequence.
"""
struct Attention
    query::Matrix{Float64}
    key::Matrix{Float64}
    value::Matrix{Float64}
end

Attention(rng::SplitMix, dim::Integer; deviation::Real = 0.1) =
    Attention(normal_matrix(rng, dim, dim, deviation), normal_matrix(rng, dim, dim, deviation), normal_matrix(rng, dim, dim, deviation))

# The softmax of each row, computed so that large scores do not overflow.
function row_softmax(scores::Matrix{Float64})
    out = similar(scores)
    for i in axes(scores, 1)
        top = maximum(@view scores[i, :])
        total = 0.0
        for j in axes(scores, 2)
            out[i, j] = exp(scores[i, j] - top)
            total += out[i, j]
        end
        @views out[i, :] ./= total
    end
    out
end

"""
    attend(layer, x)

The output of the layer and what the way back needs: the weights and the
intermediate products.
"""
function attend(layer::Attention, x::Matrix{Float64})
    scale = 1.0 / sqrt(size(x, 1))
    queries = mul(layer.query, x)
    keys = mul(layer.key, x)
    values = mul(layer.value, x)
    weights = row_softmax(mul(transposed(queries), keys) .* scale)
    (x .+ mul(values, transposed(weights)), (x = x, queries = queries, keys = keys, values = values, weights = weights, scale = scale))
end

# The gradients with respect to the input and to the three matrices, from the
# gradient with respect to the output.
function attend_back(layer::Attention, cache, dy::Matrix{Float64})
    x, weights = cache.x, cache.weights
    dvalues = mul(dy, weights)
    dweights = mul(transposed(dy), cache.values)
    dscores = similar(dweights)
    for i in axes(weights, 1)
        inner = 0.0
        for j in axes(weights, 2)
            inner += weights[i, j] * dweights[i, j]
        end
        for j in axes(weights, 2)
            dscores[i, j] = weights[i, j] * (dweights[i, j] - inner)
        end
    end
    dqueries = mul(cache.keys, transposed(dscores)) .* cache.scale
    dkeys = mul(cache.queries, dscores) .* cache.scale
    dx = dy .+ mul(transposed(layer.value), dvalues) .+ mul(transposed(layer.query), dqueries) .+ mul(transposed(layer.key), dkeys)
    xt = transposed(x)
    (dx, (query = mul(dqueries, xt), key = mul(dkeys, xt), value = mul(dvalues, xt)))
end
