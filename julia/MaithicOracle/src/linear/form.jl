# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Linear forms and rows: the normal form of linear arithmetic.

"""
    NotLinear(reason)

The statement is outside linear arithmetic, for the stated reason. The
linear oracle answers such goals with [`Unknown`](@ref), never with a guess.
"""
struct NotLinear <: Exception
    reason::String
end

Base.showerror(io::IO, e::NotLinear) = print(io, "not linear: ", e.reason)

"""
    LinearForm(coefficients, constant)

The expression `c1 * x1 + ... + cn * xn + constant` with exact coefficients;
variables whose coefficient is zero are not stored.
"""
struct LinearForm
    coefficients::Dict{String,Exact}
    constant::Exact
end

Base.:(==)(a::LinearForm, b::LinearForm) = a.coefficients == b.coefficients && a.constant == b.constant

is_constant(f::LinearForm) = isempty(f.coefficients)

function scaled(f::LinearForm, factor::Exact)
    iszero(factor) && return LinearForm(Dict{String,Exact}(), Exact(0))
    LinearForm(Dict{String,Exact}(name => c * factor for (name, c) in f.coefficients), f.constant * factor)
end

function added(a::LinearForm, b::LinearForm)
    coefficients = copy(a.coefficients)
    for (name, c) in b.coefficients
        total = get(coefficients, name, Exact(0)) + c
        iszero(total) ? delete!(coefficients, name) : (coefficients[name] = total)
    end
    LinearForm(coefficients, a.constant + b.constant)
end

"""
    linear_form(expression)

The [`LinearForm`](@ref) of an expression, or a [`NotLinear`](@ref) error.
Products are linear when one side is constant, quotients when the divisor is
a nonzero constant, powers when the base is constant or the exponent is 0
or 1; functions are not.
"""
linear_form(n::Num) = LinearForm(Dict{String,Exact}(), n.value)
linear_form(n::Var) = LinearForm(Dict{String,Exact}(n.name => Exact(1)), Exact(0))
linear_form(n::Neg) = scaled(linear_form(n.arg), Exact(-1))
linear_form(n::Call) = throw(NotLinear("the function $(n.fn) is applied"))

function linear_form(n::Pow)
    base = linear_form(n.base)
    n.exponent == 0 && return LinearForm(Dict{String,Exact}(), Exact(1))
    n.exponent == 1 && return base
    is_constant(base) || throw(NotLinear("a variable term is raised to the power $(n.exponent)"))
    LinearForm(Dict{String,Exact}(), base.constant^n.exponent)
end

function linear_form(n::BinOp)
    a, b = linear_form(n.left), linear_form(n.right)
    if n.op === :+
        added(a, b)
    elseif n.op === :-
        added(a, scaled(b, Exact(-1)))
    elseif n.op === :*
        is_constant(a) ? scaled(b, a.constant) : is_constant(b) ? scaled(a, b.constant) :
        throw(NotLinear("two variable terms are multiplied"))
    else
        is_constant(b) || throw(NotLinear("a variable term is a divisor"))
        iszero(b.constant) && throw(NotLinear("a term is divided by zero"))
        scaled(a, 1 / b.constant)
    end
end

"""
    Row(coefficients, op, bound)

One constraint in normal form: `coefficients . x op bound` with `op` one of
`:le`, `:lt`, `:eq`, the coefficients in the order of the goal's variables.
"""
struct Row
    coefficients::Vector{Exact}
    op::Symbol
    bound::Exact
end

Base.:(==)(a::Row, b::Row) = a.coefficients == b.coefficients && a.op == b.op && a.bound == b.bound

"""
    tighten(row)

The strongest row with the same integer solutions that this simple rule
gives: the coefficients are scaled to coprime integers, a bound is rounded
to the integer it implies, and a strict inequality becomes a weak one
(`2x + 4y < 7` becomes `x + 2y <= 3`). An equation whose bound cannot be met
by integers becomes the false row `0 <= -1`. This is what lets a rational
method refute integer goals; the checker applies the same rule, so it is
part of the certificate format, not of the search.
"""
function tighten(row::Row)
    all(iszero, row.coefficients) && return row
    scale = lcm(denominator.(row.coefficients))
    integers = numerator.(row.coefficients .* scale)
    divisor = gcd(integers)
    coefficients = Exact.(integers .÷ divisor)
    bound = row.bound * scale / divisor
    if row.op === :eq
        isinteger(bound) ? Row(coefficients, :eq, bound) : Row(zero(coefficients), :le, Exact(-1))
    elseif row.op === :le
        Row(coefficients, :le, Exact(floor(BigInt, bound)))
    else
        Row(coefficients, :le, Exact(ceil(BigInt, bound) - 1))
    end
end

"""
    linear_row(relation, variables, domain)

The normal form of a comparison: everything is moved to the left, `>=` and
`>` are turned around, and over the integers (`domain = :int`) the row is
tightened. A `!=` is a [`NotLinear`](@ref) error, because it stands for two
cases.
"""
function linear_row(relation::Relation, variables::AbstractVector{<:AbstractString}, domain::Symbol)
    relation.op === :ne && throw(NotLinear("a hypothesis with ≠ stands for two cases"))
    form = added(linear_form(relation.left), scaled(linear_form(relation.right), Exact(-1)))
    op = relation.op
    if op in (:ge, :gt)
        form = scaled(form, Exact(-1))
        op = op === :ge ? :le : :lt
    end
    for name in keys(form.coefficients)
        name in variables || throw(ArgumentError("the variable \"$(name)\" is used but not listed"))
    end
    row = Row(Exact[get(form.coefficients, name, Exact(0)) for name in variables], op, -form.constant)
    domain === :int ? tighten(row) : row
end

"""
    refutes(rows, multipliers)

Whether the multipliers turn the rows into a contradiction: every multiplier
of an inequality is nonnegative, the weighted sum of the left sides is the
zero form, and the weighted sum of the bounds is negative, or is zero while
a strict row has a positive multiplier. Then adding up the rows would give
`0 <= b` with `b < 0`, or `0 < 0`, so no point satisfies them all. This is
the whole of the check of a linear certificate.
"""
function refutes(rows::AbstractVector{Row}, multipliers::AbstractVector{Exact})
    (isempty(rows) || length(rows) != length(multipliers)) && return false
    width = length(first(rows).coefficients)
    all(length(r.coefficients) == width for r in rows) || return false
    all(m >= 0 || r.op === :eq for (r, m) in zip(rows, multipliers)) || return false
    for j in 1:width
        iszero(sum(m * r.coefficients[j] for (r, m) in zip(rows, multipliers))) || return false
    end
    total = sum(m * r.bound for (r, m) in zip(rows, multipliers))
    total < 0 || (iszero(total) && any(m > 0 && r.op === :lt for (r, m) in zip(rows, multipliers)))
end
