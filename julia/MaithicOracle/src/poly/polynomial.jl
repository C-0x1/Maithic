# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Multivariate polynomials with exact rational coefficients, in normal form.

"""
    NotPolynomial(reason)

The statement is outside polynomial arithmetic, for the stated reason.
"""
struct NotPolynomial <: Exception
    reason::String
end

Base.showerror(io::IO, e::NotPolynomial) = print(io, "not polynomial: ", e.reason)

"The exponents of a monomial, one per variable."
const Monomial = Vector{Int}

"""
    monomial_less(a, b)

The graded reverse lexicographic order: a monomial of smaller total degree is
smaller; among equals, the one with the larger exponent in the last variable
where they differ is smaller. With variables `x, y, z` the order begins
`1 < z < y < x < z^2 < y*z < x*z < y^2 < x*y < x^2`.
"""
function monomial_less(a::Monomial, b::Monomial)
    da, db = sum(a), sum(b)
    da != db && return da < db
    for i in length(a):-1:1
        a[i] != b[i] && return a[i] > b[i]
    end
    false
end

"""
    Polynomial

A polynomial in normal form: its terms are sorted from the largest monomial
to the smallest, no coefficient is zero and no monomial appears twice. Two
polynomials are equal exactly when their normal forms are, so comparing
expressions is comparing normal forms.
"""
struct Polynomial
    width::Int
    terms::Vector{Tuple{Monomial,Exact}}
end

Base.:(==)(a::Polynomial, b::Polynomial) = a.width == b.width && a.terms == b.terms
Base.hash(p::Polynomial, h::UInt) = hash(p.terms, hash(p.width, h))
Base.iszero(p::Polynomial) = isempty(p.terms)
Base.zero(p::Polynomial) = Polynomial(p.width, Tuple{Monomial,Exact}[])

function normal(width::Int, coefficients::Dict{Monomial,Exact})
    terms = Tuple{Monomial,Exact}[(m, c) for (m, c) in coefficients if !iszero(c)]
    sort!(terms; lt = (s, t) -> monomial_less(t[1], s[1]))
    Polynomial(width, terms)
end

constant_polynomial(width::Int, value) =
    iszero(value) ? Polynomial(width, Tuple{Monomial,Exact}[]) : Polynomial(width, [(zeros(Int, width), Exact(value))])

function variable_polynomial(width::Int, index::Int)
    exponents = zeros(Int, width)
    exponents[index] = 1
    Polynomial(width, [(exponents, Exact(1))])
end

leading_monomial(p::Polynomial) = p.terms[1][1]
leading_coefficient(p::Polynomial) = p.terms[1][2]
degree(p::Polynomial) = iszero(p) ? -1 : maximum(sum(m) for (m, _) in p.terms)

function accumulate!(coefficients::Dict{Monomial,Exact}, p::Polynomial, factor::Exact = Exact(1), shift::Union{Nothing,Monomial} = nothing)
    for (m, c) in p.terms
        key = shift === nothing ? m : m .+ shift
        coefficients[key] = get(coefficients, key, Exact(0)) + c * factor
    end
    coefficients
end

Base.:+(a::Polynomial, b::Polynomial) = normal(a.width, accumulate!(accumulate!(Dict{Monomial,Exact}(), a), b))
Base.:-(a::Polynomial, b::Polynomial) = normal(a.width, accumulate!(accumulate!(Dict{Monomial,Exact}(), a), b, Exact(-1)))
Base.:-(a::Polynomial) = Polynomial(a.width, [(m, -c) for (m, c) in a.terms])
Base.:*(a::Polynomial, factor::Union{Integer,Rational}) =
    iszero(factor) ? zero(a) : Polynomial(a.width, [(m, c * factor) for (m, c) in a.terms])
Base.:*(factor::Union{Integer,Rational}, a::Polynomial) = a * factor

function Base.:*(a::Polynomial, b::Polynomial)
    coefficients = Dict{Monomial,Exact}()
    for (m, c) in a.terms
        accumulate!(coefficients, b, c, m)
    end
    normal(a.width, coefficients)
end

function Base.:^(a::Polynomial, n::Integer)
    n >= 0 || throw(ArgumentError("a polynomial has no negative power"))
    result = constant_polynomial(a.width, 1)
    for _ in 1:n
        result *= a
    end
    result
end

"`p` times the term `factor * x^shift`."
shifted(p::Polynomial, factor::Exact, shift::Monomial) =
    Polynomial(p.width, [(m .+ shift, c * factor) for (m, c) in p.terms])

"""
    polynomial(expression, variables)

The normal form of an expression, or a [`NotPolynomial`](@ref) error. A
division is allowed when the divisor is a nonzero constant; functions and
variables that are not listed are refused.
"""
polynomial(n::Num, variables) = constant_polynomial(length(variables), n.value)
polynomial(n::Neg, variables) = -polynomial(n.arg, variables)
polynomial(n::Pow, variables) = polynomial(n.base, variables)^n.exponent
polynomial(n::Call, _) = throw(NotPolynomial("the function $(n.fn) is applied"))

function polynomial(n::Var, variables)
    index = findfirst(==(n.name), variables)
    index === nothing && throw(ArgumentError("the variable \"$(n.name)\" is used but not listed"))
    variable_polynomial(length(variables), index)
end

function polynomial(n::BinOp, variables)
    a, b = polynomial(n.left, variables), polynomial(n.right, variables)
    n.op === :+ && return a + b
    n.op === :- && return a - b
    n.op === :* && return a * b
    degree(b) == 0 || throw(NotPolynomial(iszero(b) ? "a term is divided by zero" : "a variable term is a divisor"))
    a * (1 / leading_coefficient(b))
end

"The value of a polynomial at a point, exactly."
function evaluate_polynomial(p::Polynomial, point::AbstractVector)
    total = Exact(0)
    for (m, c) in p.terms
        term = c
        for (value, e) in zip(point, m)
            e > 0 && (term *= Exact(value)^e)
        end
        total += term
    end
    total
end

"The product of `start` and the variable powers of a monomial, nested to the left so that it prints without parentheses."
function product_node(start::Union{Nothing,Node}, m::Monomial, variables; negated::Bool = false)
    node = start
    for (name, e) in zip(variables, m)
        e == 0 && continue
        factor = e == 1 ? Var(name) : Pow(Var(name), e)
        node = node === nothing ? (negated ? Neg(factor) : factor) : BinOp(:*, node, factor)
    end
    node
end

function term_node(c::Exact, m::Monomial, variables)
    all(iszero, m) && return Num(c)
    isone(c) && return product_node(nothing, m, variables)
    isone(-c) && return product_node(nothing, m, variables; negated = true)
    product_node(Num(c), m, variables)
end

"""
    polynomial_node(p, variables)

The polynomial as an expression tree in its canonical layout: terms from the
largest monomial down, a coefficient of one left unwritten, and a negative
term subtracted rather than added.
"""
function polynomial_node(p::Polynomial, variables)
    iszero(p) && return Num(Exact(0))
    m, c = p.terms[1]
    result = term_node(c, m, variables)
    for (m, c) in p.terms[2:end]
        result = BinOp(c < 0 ? :- : :+, result, term_node(abs(c), m, variables))
    end
    result
end

"""
    polynomial_text(p, variables; style = :plain)

The canonical text of a polynomial, such as `x^2 - 2 * x * y + 3 / 4`. Equal
polynomials have equal text.
"""
polynomial_text(p::Polynomial, variables; style::Symbol = :plain) =
    expression_text(polynomial_node(p, variables); style)
