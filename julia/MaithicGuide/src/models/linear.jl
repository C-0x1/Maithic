# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Products of small matrices, written out.
#
# Julia hands matrix products to a numerical library that picks its method by
# the processor it finds, and two methods add the same numbers in a different
# order. The difference is in the last digit, and after a few thousand steps
# of training it is a different model. The models here are small, so the
# products are plain loops in a fixed order: slower than the library, and the
# same on every machine.

"`a * b` for matrices."
function mul(a::AbstractMatrix{Float64}, b::AbstractMatrix{Float64})
    size(a, 2) == size(b, 1) || throw(DimensionMismatch("a matrix with $(size(a, 2)) columns cannot be multiplied by one with $(size(b, 1)) rows"))
    out = zeros(size(a, 1), size(b, 2))
    @inbounds for j in axes(b, 2), k in axes(a, 2)
        scale = b[k, j]
        for i in axes(a, 1)
            out[i, j] += a[i, k] * scale
        end
    end
    out
end

"`a * x` for a matrix and a vector."
function mul(a::AbstractMatrix{Float64}, x::AbstractVector{Float64})
    size(a, 2) == length(x) || throw(DimensionMismatch("a matrix with $(size(a, 2)) columns cannot be multiplied by a vector of $(length(x))"))
    out = zeros(size(a, 1))
    @inbounds for k in axes(a, 2)
        scale = x[k]
        for i in axes(a, 1)
            out[i] += a[i, k] * scale
        end
    end
    out
end

"The transpose of a matrix, as a matrix of its own."
transposed(a::AbstractMatrix{Float64}) = permutedims(a)

"Adds the outer product `a * b'` to `target`."
function add_outer!(target::Matrix{Float64}, a::AbstractVector{Float64}, b::AbstractVector{Float64})
    size(target) == (length(a), length(b)) || throw(DimensionMismatch("the outer product does not have the shape of its target"))
    @inbounds for j in eachindex(b), i in eachindex(a)
        target[i, j] += a[i] * b[j]
    end
    target
end
