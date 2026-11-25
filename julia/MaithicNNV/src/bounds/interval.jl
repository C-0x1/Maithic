# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Closed intervals and the arithmetic of bound propagation.
#
# With exact endpoints the operations are exact. With floating-point endpoints
# every computed endpoint is moved one unit in the last place outwards, which
# is enough to enclose the exact result of an operation rounded to nearest.

"A closed interval `[lo, hi]`."
struct Interval{T<:Real}
    lo::T
    hi::T
    function Interval{T}(lo::T, hi::T) where {T<:Real}
        (isnan(lo) || isnan(hi)) && throw(ArgumentError("interval endpoints must be numbers"))
        lo <= hi || throw(ArgumentError("interval lower bound $lo exceeds upper bound $hi"))
        new{T}(lo, hi)
    end
end

Interval(lo::T, hi::T) where {T<:Real} = Interval{T}(lo, hi)
Interval(lo::Real, hi::Real) = Interval(promote(lo, hi)...)
Interval(x::Real) = Interval(x, x)

"A box: one interval per coordinate."
const Box{T} = Vector{Interval{T}}

lo(i::Interval) = i.lo
hi(i::Interval) = i.hi
width(i::Interval) = i.hi - i.lo
encloses(i::Interval, x::Real) = i.lo <= x <= i.hi
encloses(b::AbstractVector{<:Interval}, x::AbstractVector{<:Real}) =
    length(b) == length(x) && all(encloses(b[k], x[k]) for k in eachindex(b))

"`true` when the first interval lies inside the second."
is_subset(a::Interval, b::Interval) = b.lo <= a.lo && a.hi <= b.hi
is_subset(a::AbstractVector{<:Interval}, b::AbstractVector{<:Interval}) =
    length(a) == length(b) && all(is_subset(a[k], b[k]) for k in eachindex(a))

Base.:(==)(a::Interval, b::Interval) = a.lo == b.lo && a.hi == b.hi
Base.hash(i::Interval, h::UInt) = hash(i.hi, hash(i.lo, hash(:Interval, h)))
Base.show(io::IO, i::Interval) = print(io, "[", i.lo, ", ", i.hi, "]")

# Outward rounding. Exact types need none.
down(x::AbstractFloat) = isfinite(x) ? prevfloat(x) : x
up(x::AbstractFloat) = isfinite(x) ? nextfloat(x) : x
down(x::Real) = x
up(x::Real) = x

"The interval sum, enclosing `x + y` for `x` in `a` and `y` in `b`."
add(a::Interval{T}, b::Interval{T}) where {T} = Interval{T}(down(a.lo + b.lo), up(a.hi + b.hi))

"The interval enclosing `c * x` for `x` in `a`."
function scale(c::T, a::Interval{T}) where {T}
    p, q = c * a.lo, c * a.hi
    Interval{T}(down(min(p, q)), up(max(p, q)))
end

"The interval enclosing `x + c` for `x` in `a`."
shift(c::T, a::Interval{T}) where {T} = Interval{T}(down(a.lo + c), up(a.hi + c))

"The image of an interval under an activation. Monotone, so no rounding is needed."
activate(act::Activation, a::Interval{T}) where {T} =
    act == ReLU ? Interval{T}(max(zero(T), a.lo), max(zero(T), a.hi)) : a

"The interval dot product of a weight row with the common prefix of a box."
function dot_prefix(w::AbstractVector{T}, b::AbstractVector{Interval{T}}) where {T}
    acc = Interval{T}(zero(T), zero(T))
    for k in 1:min(length(w), length(b))
        acc = add(acc, scale(w[k], b[k]))
    end
    acc
end

"""
    enclose(interval)

An exact rational interval that contains a floating-point one. The endpoints
are converted exactly, so nothing is lost and nothing needs to be assumed about
how they were printed or parsed.
"""
enclose(i::Interval{<:AbstractFloat}) = begin
    (isfinite(i.lo) && isfinite(i.hi)) || throw(ArgumentError("cannot enclose an unbounded interval"))
    Interval{Exact}(Exact(i.lo), Exact(i.hi))
end
enclose(i::Interval) = Interval{Exact}(Exact(i.lo), Exact(i.hi))
enclose(b::AbstractVector{<:Interval}) = [enclose(i) for i in b]
