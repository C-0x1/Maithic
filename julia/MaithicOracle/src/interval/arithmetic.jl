# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Interval arithmetic with outward rounding.
#
# An operation on intervals returns an interval that contains every value the
# real operation can take on the inputs. Each endpoint is therefore rounded in
# its own direction: lower endpoints down, upper endpoints up. A result that
# is exactly representable is not moved at all, so arithmetic on integers and
# other exact data stays exact, and a bound that is attained, as in
# x * (1 - x) >= 0 on [0, 1], can still be proved.
#
# For BigFloat the directed rounding is MPFR's own. For Float64 it is
# recovered from the rounded result and the exact rounding error, which
# error-free transformations compute (Knuth's two-sum, and a fused
# multiply-add for products, quotients and square roots).

"""
    RealInterval{T}(lo, hi)

The closed interval `[lo, hi]` of real numbers with endpoints of the
floating-point type `T`. `Float64` is used for speed in searches and
`BigFloat` for the independent second check of a certificate. An endpoint
may be infinite; it then stands for "no bound known".
"""
struct RealInterval{T<:AbstractFloat}
    lo::T
    hi::T
    function RealInterval{T}(lo::T, hi::T) where {T<:AbstractFloat}
        (isnan(lo) || isnan(hi) || lo > hi) && throw(ArgumentError("[$(lo), $(hi)] is not an interval"))
        new{T}(lo, hi)
    end
end

RealInterval(lo::T, hi::T) where {T<:AbstractFloat} = RealInterval{T}(lo, hi)

"""
    enclose(T, lo, hi = lo)

The smallest interval with endpoints of type `T` that contains the exact
numbers `lo` and `hi`: the lower endpoint is rounded down and the upper one
up.
"""
enclose(::Type{T}, lo, hi = lo) where {T<:AbstractFloat} = RealInterval{T}(T(exact(lo), RoundDown), T(exact(hi), RoundUp))

Base.:(==)(a::RealInterval, b::RealInterval) = a.lo == b.lo && a.hi == b.hi
Base.in(x::Real, a::RealInterval) = a.lo <= x <= a.hi
Base.issubset(a::RealInterval, b::RealInterval) = b.lo <= a.lo && a.hi <= b.hi
Base.show(io::IO, a::RealInterval) = print(io, "[", a.lo, ", ", a.hi, "]")

"The width of an interval, rounded up."
width(a::RealInterval) = sum_bounds(a.hi, -a.lo)[2]

"""
    Undefined(what)

An operation was applied to an interval on part of which it has no value:
a division by an interval that contains zero, a square root or a logarithm
that reaches below its domain. The proof search answers by splitting the box.
"""
struct Undefined <: Exception
    what::String
end

Base.showerror(io::IO, e::Undefined) = print(io, e.what, " is not defined on the whole interval")

"Runs `f` with BigFloat arithmetic rounding in the given direction."
directed(f, mode::RoundingMode) = setrounding(f, BigFloat, mode)

overflowed(value::Float64) = value > 0 ? (floatmax(Float64), Inf) : (-Inf, -floatmax(Float64))
around(value::Float64, error::Float64) =
    iszero(error) ? (value, value) : error > 0 ? (value, nextfloat(value)) : (prevfloat(value), value)

"Below this magnitude a rounding error may itself be lost to underflow, and both neighbours are taken instead."
const SAFE_MAGNITUDE = 2.0^-900

"""
    sum_bounds(x, y), product_bounds(x, y), quotient_bounds(x, y), root_bounds(x)

The floating-point numbers just below and just above the exact result of the
operation, equal to each other when the result is representable. An
infinite argument stands for a missing bound and gives a missing bound; zero
times a missing bound is zero.
"""
function sum_bounds(x::Float64, y::Float64)
    s = x + y
    isfinite(s) || return (isfinite(x) && isfinite(y)) ? overflowed(s) : (s, s)
    partner = s - x
    around(s, (x - (s - partner)) + (y - partner))
end

function product_bounds(x::Float64, y::Float64)
    (iszero(x) || iszero(y)) && return (0.0, 0.0)
    p = x * y
    isfinite(p) || return (isfinite(x) && isfinite(y)) ? overflowed(p) : (p, p)
    abs(p) < SAFE_MAGNITUDE ? (prevfloat(p), nextfloat(p)) : around(p, fma(x, y, -p))
end

function quotient_bounds(x::Float64, y::Float64)
    (iszero(x) || isinf(y)) && return (0.0, 0.0)
    q = x / y
    isfinite(q) || return isfinite(x) ? overflowed(q) : (q, q)
    (abs(q) < SAFE_MAGNITUDE || abs(x) < SAFE_MAGNITUDE) && return (prevfloat(q), nextfloat(q))
    remainder = fma(-q, y, x)
    around(q, y > 0 ? remainder : -remainder)
end

function root_bounds(x::Float64)
    (iszero(x) || isinf(x)) && return (x, x)
    s = sqrt(x)
    x < SAFE_MAGNITUDE ? (prevfloat(s), nextfloat(s)) : around(s, fma(-s, s, x))
end

sum_bounds(x::BigFloat, y::BigFloat) = (directed(() -> x + y, RoundDown), directed(() -> x + y, RoundUp))
product_bounds(x::BigFloat, y::BigFloat) =
    (iszero(x) || iszero(y)) ? (zero(x), zero(x)) : (directed(() -> x * y, RoundDown), directed(() -> x * y, RoundUp))
quotient_bounds(x::BigFloat, y::BigFloat) =
    (iszero(x) || isinf(y)) ? (zero(x), zero(x)) : (directed(() -> x / y, RoundDown), directed(() -> x / y, RoundUp))
root_bounds(x::BigFloat) = (directed(() -> sqrt(x), RoundDown), directed(() -> sqrt(x), RoundUp))

Base.:-(a::RealInterval{T}) where {T} = RealInterval{T}(-a.hi, -a.lo)
Base.:+(a::RealInterval{T}, b::RealInterval{T}) where {T} =
    RealInterval{T}(sum_bounds(a.lo, b.lo)[1], sum_bounds(a.hi, b.hi)[2])
Base.:-(a::RealInterval{T}, b::RealInterval{T}) where {T} = a + (-b)

function Base.:*(a::RealInterval{T}, b::RealInterval{T}) where {T}
    corners = (product_bounds(a.lo, b.lo), product_bounds(a.lo, b.hi), product_bounds(a.hi, b.lo), product_bounds(a.hi, b.hi))
    RealInterval{T}(minimum(first, corners), maximum(last, corners))
end

function Base.inv(a::RealInterval{T}) where {T}
    (a.lo <= 0 <= a.hi) && throw(Undefined("a division by an interval that contains zero"))
    RealInterval{T}(quotient_bounds(one(T), a.hi)[1], quotient_bounds(one(T), a.lo)[2])
end

Base.:/(a::RealInterval{T}, b::RealInterval{T}) where {T} = a * inv(b)

"An upper bound of `x^n` for `x >= 0`, by multiplications that each round up."
power_up(x::T, n::Int) where {T<:AbstractFloat} = foldl((result, _) -> product_bounds(result, x)[2], 1:n; init = one(T))

"A lower bound of `x^n` for `x >= 0`, by multiplications that each round down."
power_down(x::T, n::Int) where {T<:AbstractFloat} =
    foldl((result, _) -> max(zero(T), product_bounds(result, x)[1]), 1:n; init = one(T))

function Base.:^(a::RealInterval{T}, n::Integer) where {T}
    n >= 0 || throw(ArgumentError("an exponent must be a natural number"))
    n = Int(n)
    n == 0 && return RealInterval{T}(one(T), one(T))
    if a.lo >= 0
        RealInterval{T}(power_down(a.lo, n), power_up(a.hi, n))
    elseif a.hi <= 0
        positive = RealInterval{T}(power_down(-a.hi, n), power_up(-a.lo, n))
        iseven(n) ? positive : -positive
    elseif iseven(n)
        RealInterval{T}(zero(T), power_up(max(-a.lo, a.hi), n))
    else
        RealInterval{T}(-power_up(-a.lo, n), power_up(a.hi, n))
    end
end

Base.abs(a::RealInterval{T}) where {T} =
    a.lo >= 0 ? a : a.hi <= 0 ? -a : RealInterval{T}(zero(T), max(-a.lo, a.hi))
Base.min(a::RealInterval{T}, b::RealInterval{T}) where {T} = RealInterval{T}(min(a.lo, b.lo), min(a.hi, b.hi))
Base.max(a::RealInterval{T}, b::RealInterval{T}) where {T} = RealInterval{T}(max(a.lo, b.lo), max(a.hi, b.hi))

function Base.sqrt(a::RealInterval{T}) where {T}
    a.lo < 0 && throw(Undefined("a square root of an interval that reaches below zero"))
    RealInterval{T}(root_bounds(a.lo)[1], root_bounds(a.hi)[2])
end

# The other elementary functions. MPFR, behind BigFloat, rounds every function
# correctly in the direction asked for. For Float64 the function is computed
# in BigFloat and the bound is rounded once more in the same direction,
# because the system mathematics library gives no such guarantee.
below(f, x::BigFloat) = directed(() -> f(x), RoundDown)
above(f, x::BigFloat) = directed(() -> f(x), RoundUp)
below(f, x::Float64) = isfinite(x) ? Float64(directed(() -> f(BigFloat(x)), RoundDown), RoundDown) : f(x)
above(f, x::Float64) = isfinite(x) ? Float64(directed(() -> f(BigFloat(x)), RoundUp), RoundUp) : f(x)

Base.exp(a::RealInterval{T}) where {T} = RealInterval{T}(max(zero(T), below(exp, a.lo)), above(exp, a.hi))

function Base.log(a::RealInterval{T}) where {T}
    a.lo <= 0 && throw(Undefined("a logarithm of an interval that reaches zero"))
    RealInterval{T}(below(log, a.lo), above(log, a.hi))
end

"An interval that contains pi, in BigFloat arithmetic."
big_pi() = RealInterval{BigFloat}(directed(() -> BigFloat(pi), RoundDown), directed(() -> BigFloat(pi), RoundUp))

big_interval(x::Integer) = RealInterval{BigFloat}(BigFloat(x), BigFloat(x))

"""
Whether some point `(numerator / denominator) * pi + 2 * k * pi`, `k` an
integer, may lie in `[lo, hi]`. The answer errs on the side of `true`: when
it is `false` there is certainly no such point.
"""
function may_hit(lo::AbstractFloat, hi::AbstractFloat, numerator::Int, denominator::Int)
    period = big_interval(2) * big_pi()
    offset = big_interval(numerator) * big_pi() / big_interval(denominator)
    first_turn = (RealInterval{BigFloat}(BigFloat(lo), BigFloat(lo)) - offset) / period
    last_turn = (RealInterval{BigFloat}(BigFloat(hi), BigFloat(hi)) - offset) / period
    ceil(first_turn.lo) <= floor(last_turn.hi)
end

function periodic(f, a::RealInterval{T}, maximum_at::Tuple{Int,Int}, minimum_at::Tuple{Int,Int}) where {T}
    (isfinite(a.lo) && isfinite(a.hi)) || return RealInterval{T}(-one(T), one(T))
    hi = may_hit(a.lo, a.hi, maximum_at...) ? one(T) : min(one(T), max(above(f, a.lo), above(f, a.hi)))
    lo = may_hit(a.lo, a.hi, minimum_at...) ? -one(T) : max(-one(T), min(below(f, a.lo), below(f, a.hi)))
    RealInterval{T}(lo, hi)
end

# Between two neighbouring extrema a sine or cosine is monotone, so when no
# extremum can lie in the interval the range is spanned by the endpoints.
Base.sin(a::RealInterval) = periodic(sin, a, (1, 2), (-1, 2))
Base.cos(a::RealInterval) = periodic(cos, a, (0, 1), (1, 1))

# Evaluation of expressions over intervals plugs into MaithicCore.evaluate.
MaithicCore.lift(::Type{RealInterval{T}}, x::Union{Integer,Rational}) where {T} = enclose(T, x)
MaithicCore.divide(a::RealInterval, b::RealInterval) = a / b
MaithicCore.power(a::RealInterval, n::Int) = a^n
for fn in (:sqrt, :exp, :log, :sin, :cos)
    @eval MaithicCore.apply(::Val{$(QuoteNode(fn))}, a::RealInterval) = $fn(a)
end

"""
    enclosure(expression, box, T = Float64)

An interval that contains every value the expression takes when each
variable ranges over its interval in `box`, a list of `name => (lo, hi)`
with exact bounds. An [`Undefined`](@ref) error means that the expression
may have no value somewhere on the box.
"""
function enclosure(node::Node, box, ::Type{T} = Float64) where {T<:AbstractFloat}
    env = Dict{String,RealInterval{T}}(name => enclose(T, lo, hi) for (name, (lo, hi)) in box)
    evaluate(node, env, RealInterval{T})
end

"The exact rational endpoints of an interval with finite endpoints."
exact_bounds(a::RealInterval) = (Exact(a.lo), Exact(a.hi))
