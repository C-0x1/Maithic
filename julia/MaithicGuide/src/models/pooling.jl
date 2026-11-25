# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# From the vectors of a sequence to one vector.

const POOLINGS = (:mean, :first)

"""
    pool(kind, x)

One vector for a sequence whose vectors are the columns of `x`: their mean
with `:mean`, the first of them with `:first`. The first vector says
something about the whole sequence only after a layer that lets the tokens
see each other, which is what [`Attention`](@ref) is for.
"""
function pool(kind::Symbol, x::AbstractMatrix{Float64})
    size(x, 2) >= 1 || throw(ArgumentError("there is nothing to pool in an empty sequence"))
    if kind === :mean
        h = zeros(size(x, 1))
        @inbounds for j in axes(x, 2), i in axes(x, 1)
            h[i] += x[i, j]
        end
        return h ./ size(x, 2)
    end
    kind === :first && return x[:, 1]
    throw(ArgumentError("there is no pooling called $(kind); the poolings are $(join(POOLINGS, " and "))"))
end

# The gradient with respect to the vectors of the sequence, from the gradient
# with respect to the pooled vector.
function pool_back(kind::Symbol, x::AbstractMatrix{Float64}, dh::AbstractVector{Float64})
    dx = zeros(size(x))
    if kind === :mean
        dx .= dh ./ size(x, 2)
    elseif kind === :first
        dx[:, 1] .= dh
    else
        throw(ArgumentError("there is no pooling called $(kind)"))
    end
    dx
end
