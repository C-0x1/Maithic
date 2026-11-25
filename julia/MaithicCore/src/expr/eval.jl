# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Evaluating expressions over any number type.

"A variable of the expression has no value in the environment."
struct UnboundVariable <: Exception
    name::String
end

Base.showerror(io::IO, e::UnboundVariable) = print(io, "the variable \"", e.name, "\" has no value")

"A function such as `sqrt` has no exact rational value in general."
struct InexactFunction <: Exception
    fn::Symbol
end

Base.showerror(io::IO, e::InexactFunction) =
    print(io, e.fn, " has no exact rational value in general; evaluate in floating point or with intervals")

"""
    lift(T, x)

`x` as a number of type `T`. Evaluation calls it on every literal and every
value taken from the environment. A package that evaluates expressions over
its own number type, such as intervals that must round outward, adds a
method.
"""
lift(::Type{T}, x::T) where {T} = x
lift(::Type{T}, x) where {T} = convert(T, x)

"""
    evaluate(node, env, T = Exact)

The value of an expression, computed in the number type `T`. `env` maps
variable names to values. With the default `T` the arithmetic is exact: a
division by zero is a `DivideError` and the transcendental functions are an
[`InexactFunction`](@ref) error, never a rounded answer.
"""
evaluate(node::Node, env) = evaluate(node, env, Exact)
evaluate(n::Num, _, ::Type{T}) where {T} = lift(T, n.value)
evaluate(n::Var, env, ::Type{T}) where {T} =
    haskey(env, n.name) ? lift(T, env[n.name]) : throw(UnboundVariable(n.name))
evaluate(n::Neg, env, ::Type{T}) where {T} = -evaluate(n.arg, env, T)
evaluate(n::Pow, env, ::Type{T}) where {T} = power(evaluate(n.base, env, T), n.exponent)
evaluate(n::Call, env, ::Type{T}) where {T} = apply(Val(n.fn), (evaluate(arg, env, T) for arg in n.args)...)

function evaluate(n::BinOp, env, ::Type{T}) where {T}
    a = evaluate(n.left, env, T)
    b = evaluate(n.right, env, T)
    n.op === :+ ? a + b : n.op === :- ? a - b : n.op === :* ? a * b : divide(a, b)
end

divide(a, b) = a / b
divide(a::Rational, b::Rational) = iszero(b) ? throw(DivideError()) : a / b
power(a, n::Int) = a^n

apply(::Val{:abs}, x) = abs(x)
apply(::Val{:min}, x, y) = min(x, y)
apply(::Val{:max}, x, y) = max(x, y)
apply(::Val{:exp}, x::AbstractFloat) = exp(x)
apply(::Val{:sin}, x::AbstractFloat) = sin(x)
apply(::Val{:cos}, x::AbstractFloat) = cos(x)
apply(::Val{:sqrt}, x::AbstractFloat) = x < 0 ? throw(DomainError(x, "sqrt of a negative number")) : sqrt(x)
apply(::Val{:log}, x::AbstractFloat) = x <= 0 ? throw(DomainError(x, "log of a number that is not positive")) : log(x)

for fn in (:sqrt, :exp, :log, :sin, :cos)
    @eval apply(::Val{$(QuoteNode(fn))}, ::Rational) = throw(InexactFunction($(QuoteNode(fn))))
end

compare(op::Symbol, a, b) =
    op === :le ? a <= b : op === :lt ? a < b : op === :ge ? a >= b : op === :gt ? a > b :
    op === :eq ? a == b : op === :ne ? a != b : throw(ArgumentError("unknown comparison $(op)"))

"""
    holds(relation, env, T = Exact)

Whether a relation is true at a point, with both sides computed in `T`.
"""
holds(r::Relation, env, ::Type{T} = Exact) where {T} =
    compare(r.op, evaluate(r.left, env, T), evaluate(r.right, env, T))
