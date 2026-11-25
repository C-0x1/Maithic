# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Certificates of polynomial arithmetic, and the oracle that finds them.

"""
    PolynomialCertificate(domain, variables, hypotheses, goal, cofactors)

The equation `goal` follows from the equations `hypotheses`, because the
goal, as `left - right`, is the sum of each hypothesis, as `left - right`,
times its cofactor. Where the hypotheses hold their differences are zero, so
the goal's difference is zero. With no hypotheses the certificate says the
goal is an identity: its two sides have the same normal form.
"""
struct PolynomialCertificate <: Certificate
    domain::Symbol
    variables::Vector{String}
    hypotheses::Vector{Hypothesis}
    goal::Relation
    cofactors::Vector{Polynomial}
end

kind(c::PolynomialCertificate) = isempty(c.hypotheses) ? "polynomial.identity" : "polynomial.combination"

const DOMAIN_SYMBOLS = Dict(:int => "ℤ", :rat => "ℚ", :real => "ℝ")

function claim(c::PolynomialCertificate)
    steps = String[relation_text(h.statement; style = :lean) for h in c.hypotheses]
    push!(steps, relation_text(c.goal; style = :lean))
    body = join(steps, " → ")
    isempty(c.variables) ? body : "∀ $(join(c.variables, " ")) : $(get(DOMAIN_SYMBOLS, c.domain, "?")), $(body)"
end

certificate_data(c::PolynomialCertificate) =
    Object("domain" => String(c.domain), "variables" => c.variables,
           "hypotheses" => Any[Object("name" => h.name, "statement" => relation_text(h.statement)) for h in c.hypotheses],
           "goal" => relation_text(c.goal),
           "cofactors" => Any[polynomial_text(q, c.variables) for q in c.cofactors])

difference(r::Relation, variables) = polynomial(r.left, variables) - polynomial(r.right, variables)

function check(c::PolynomialCertificate)
    (c.domain in (:int, :rat, :real) && allunique(c.variables) && allunique(h.name for h in c.hypotheses)) || return false
    (c.goal.op === :eq && all(h.statement.op === :eq for h in c.hypotheses)) || return false
    length(c.cofactors) == length(c.hypotheses) || return false
    all(q.width == length(c.variables) for q in c.cofactors) || return false
    try
        combination = constant_polynomial(length(c.variables), 0)
        for (h, q) in zip(c.hypotheses, c.cofactors)
            combination += q * difference(h.statement, c.variables)
        end
        combination == difference(c.goal, c.variables)
    catch e
        e isa Union{NotPolynomial,ArgumentError} || rethrow()
        false
    end
end

difference_node(r::Relation) = BinOp(:-, r.left, r.right)

"""
    lean_statement(certificate::PolynomialCertificate)

The arithmetic of the certificate as one ring identity for Lean's `grind`.
For an identity that is the goal itself. For a combination it is the
equation the certificate asserts: the goal's `left - right` equals the sum
of the cofactors times each hypothesis's `left - right`, with the statements
written as they were given, so that the kernel checks the certificate
against the original text and not against a normal form computed here.

Over the integers the equation is first multiplied by the common denominator
of the cofactors, so that every coefficient is an integer. The text is
`nothing` over the reals, which core Lean does not have, and over the
integers when a statement contains a division or a number that is not an
integer.
"""
function lean_statement(c::PolynomialCertificate)
    c.domain === :real && return nothing
    statements = Relation[h.statement for h in c.hypotheses]
    push!(statements, c.goal)
    c.domain === :int && !all(r -> lean_exact(r, :int), statements) && return nothing
    type = c.domain === :int ? "Int" : "Rat"
    binders = isempty(c.variables) ? "" : " ($(join(c.variables, " ")) : $(type))"
    isempty(c.hypotheses) && return "example$(binders) : $(typed_relation(c.goal, type)) := by grind\n"
    scale = c.domain === :int ?
            foldl(lcm, (denominator(coefficient) for q in c.cofactors for (_, coefficient) in q.terms); init = big(1)) : big(1)
    left = scale == 1 ? difference_node(c.goal) : BinOp(:*, Num(Exact(scale)), difference_node(c.goal))
    right = nothing
    for (h, q) in zip(c.hypotheses, c.cofactors)
        iszero(q) && continue
        term = BinOp(:*, polynomial_node(q * scale, c.variables), difference_node(h.statement))
        right = right === nothing ? term : BinOp(:+, right, term)
    end
    right === nothing && (right = Num(Exact(0)))
    string("example", binders, " : (", expression_text(left; style = :lean), " : ", type, ") = ",
           expression_text(right; style = :lean), " := by grind\n")
end

function polynomial_certificate_of(d)
    variables = String[text_of(v) for v in list_of(d["variables"])]
    PolynomialCertificate(Symbol(text_of(d["domain"])), variables,
                          Hypothesis[hypothesis_of(h) for h in list_of(d["hypotheses"])], relation_of(d["goal"]),
                          Polynomial[polynomial(parse_expression(text_of(q)), variables) for q in list_of(d["cofactors"])])
end

READERS["polynomial.identity"] = polynomial_certificate_of
READERS["polynomial.combination"] = polynomial_certificate_of

"""
    nonroot(p)

A point with natural-number coordinates at which the nonzero polynomial `p`
does not vanish. One exists with each coordinate at most the degree of `p`
in that variable, so the search is finite and always succeeds.
"""
function nonroot(p::Polynomial)
    bounds = [maximum(m[i] for (m, _) in p.terms) for i in 1:p.width]
    point = zeros(Int, p.width)
    while true
        iszero(evaluate_polynomial(p, point)) || return copy(point)
        position = p.width
        while position >= 1 && point[position] == bounds[position]
            point[position] = 0
            position -= 1
        end
        position == 0 && throw(ErrorException("a nonzero polynomial vanished on its whole grid"))
        point[position] += 1
    end
end

"""
    prove_polynomial(goal; steps = 200_000)

Decides an equation between polynomials.

- With no hypotheses the answer is always definite: [`Proved`](@ref) when
  both sides have the same normal form, [`Refuted`](@ref) with a point at
  which they differ otherwise.
- With hypotheses, all of them equations, the goal is proved when it lies in
  their ideal, with cofactors from Buchberger's algorithm. Otherwise the
  outcome is [`Unknown`](@ref): a goal outside the ideal may still follow
  from the hypotheses (from `x^2 = 0`, `x = 0` follows, but `x` is not a
  multiple of `x^2`).

`steps` bounds the reduction steps of the computation.
"""
function prove_polynomial(g::Goal; steps::Int = 200_000)
    g.goal === nothing && return Unknown("the polynomial oracle proves equations, not contradictions")
    g.goal.op === :eq || return Unknown("the polynomial oracle proves equations, and the goal is not one")
    local target, generators
    try
        target = difference(g.goal, g.variables)
    catch e
        e isa NotPolynomial || rethrow()
        return Unknown("the goal is $(sprint(showerror, e))")
    end
    generators = Polynomial[]
    for h in g.hypotheses
        h.statement.op === :eq || return Unknown("hypothesis $(h.name) is not an equation")
        try
            push!(generators, difference(h.statement, g.variables))
        catch e
            e isa NotPolynomial || rethrow()
            return Unknown("hypothesis $(h.name) is $(sprint(showerror, e))")
        end
    end
    if isempty(generators) && !iszero(target)
        model = Pair{String,Exact}[name => Exact(value) for (name, value) in zip(g.variables, nonroot(target))]
        is_counterexample(g, model) || throw(ErrorException("the polynomial oracle produced a model that fails its check"))
        return Refuted(model)
    end
    cofactors = membership(target, generators; budget = Budget(steps))
    cofactors === :budget && return Unknown("the Gröbner basis computation exceeded its budget of $(steps) steps")
    cofactors === nothing && return Unknown("the goal is not a polynomial combination of the hypotheses")
    certificate = PolynomialCertificate(g.domain, g.variables, g.hypotheses, g.goal, cofactors)
    check(certificate) || throw(RejectedCertificate(kind(certificate)))
    Proved(certificate)
end

"""
    prove_polynomial(hypotheses, goal; domain = :rat, variables = nothing, steps = 200_000)

The same, from statements as text:
`prove_polynomial(["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5")`.
"""
prove_polynomial(hypotheses::AbstractVector, goal; domain::Symbol = :rat, variables = nothing, steps::Int = 200_000) =
    prove_polynomial(Goal(:polynomial, hypotheses, goal; domain, variables); steps)
