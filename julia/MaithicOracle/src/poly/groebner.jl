# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Division and Buchberger's algorithm, with cofactors.
#
# Every polynomial the algorithm derives is kept together with its expression
# as a combination of the generators. When a goal reduces to zero, those
# expressions add up to the certificate: the goal as an explicit combination
# of the hypotheses, which is checked by expanding it.

"""
    Budget(steps)

A counter of reduction steps. The computations here can grow without warning,
so each one draws on a budget and gives up cleanly when it is spent. The
budget counts steps, never seconds: the same input stops at the same place
on every machine.
"""
mutable struct Budget
    steps::Int
end

struct BudgetSpent <: Exception end

spend!(budget::Budget) = (budget.steps -= 1) < 0 ? throw(BudgetSpent()) : nothing

divides(a::Monomial, b::Monomial) = all(a .<= b)

"""
    divide(p, divisors, budget = Budget(typemax(Int)))

Multivariate division: quotients `q` and a remainder `r` with
`p = sum(q[k] * divisors[k]) + r`, where no term of `r` is divisible by the
leading monomial of a divisor. At each step the first divisor that applies is
used.
"""
function divide(p::Polynomial, divisors::AbstractVector{Polynomial}, budget::Budget = Budget(typemax(Int)))
    quotients = [Dict{Monomial,Exact}() for _ in divisors]
    remainder = Tuple{Monomial,Exact}[]
    work = p
    while !iszero(work)
        spend!(budget)
        m, c = work.terms[1]
        k = findfirst(d -> divides(leading_monomial(d), m), divisors)
        if k === nothing
            push!(remainder, (m, c))
            work = Polynomial(work.width, work.terms[2:end])
        else
            shift = m .- leading_monomial(divisors[k])
            factor = c / leading_coefficient(divisors[k])
            quotients[k][shift] = get(quotients[k], shift, Exact(0)) + factor
            work = work - shifted(divisors[k], factor, shift)
        end
    end
    ([normal(p.width, q) for q in quotients], Polynomial(p.width, remainder))
end

"A polynomial of the ideal together with its expression in the generators."
struct Tracked
    polynomial::Polynomial
    cofactors::Vector{Polynomial}
end

function monic(t::Tracked)
    scale = 1 / leading_coefficient(t.polynomial)
    Tracked(t.polynomial * scale, [c * scale for c in t.cofactors])
end

"""
    groebner(generators; budget = Budget(200_000), max_basis = 200)

A Gröbner basis of the ideal of the generators for the graded reverse
lexicographic order, by Buchberger's algorithm: S-polynomials are formed for
pairs of basis elements, smallest least common multiple first, reduced, and
added when they do not vanish. Pairs with coprime leading monomials are
skipped, as are pairs made redundant by a third element (the chain
criterion).

Returns the basis as [`Tracked`](@ref) elements, each with its cofactors in
the generators, or `nothing` when the budget is spent or the basis outgrows
`max_basis`.
"""
function groebner(generators::AbstractVector{Polynomial}; budget::Budget = Budget(200_000), max_basis::Int = 200)
    isempty(generators) && return Tracked[]
    width = first(generators).width
    unit(i) = [i == j ? constant_polynomial(width, 1) : constant_polynomial(width, 0) for j in eachindex(generators)]
    basis = Tracked[monic(Tracked(g, unit(i))) for (i, g) in enumerate(generators) if !iszero(g)]
    pending = Set{Tuple{Int,Int}}((i, j) for j in eachindex(basis) for i in 1:(j - 1))
    lcm_of(i, j) = max.(leading_monomial(basis[i].polynomial), leading_monomial(basis[j].polynomial))
    ordered(i, j) = i < j ? (i, j) : (j, i)
    try
        while !isempty(pending)
            pair = argmin(p -> (sum(lcm_of(p...)), p[2], p[1]), pending)
            delete!(pending, pair)
            i, j = pair
            common = lcm_of(i, j)
            mi, mj = leading_monomial(basis[i].polynomial), leading_monomial(basis[j].polynomial)
            sum(common) == sum(mi) + sum(mj) && continue
            any(k -> k != i && k != j && divides(leading_monomial(basis[k].polynomial), common) &&
                     !(ordered(i, k) in pending) && !(ordered(j, k) in pending), eachindex(basis)) && continue
            one = Exact(1)
            s = shifted(basis[i].polynomial, one, common .- mi) - shifted(basis[j].polynomial, one, common .- mj)
            quotients, remainder = divide(s, [b.polynomial for b in basis], budget)
            iszero(remainder) && continue
            cofactors = [shifted(basis[i].cofactors[g], one, common .- mi) - shifted(basis[j].cofactors[g], one, common .- mj)
                         for g in eachindex(generators)]
            for (k, q) in enumerate(quotients)
                iszero(q) && continue
                for g in eachindex(generators)
                    cofactors[g] -= q * basis[k].cofactors[g]
                end
            end
            push!(basis, monic(Tracked(remainder, cofactors)))
            length(basis) > max_basis && return nothing
            for k in 1:(length(basis) - 1)
                push!(pending, (k, length(basis)))
            end
        end
    catch e
        e isa BudgetSpent || rethrow()
        return nothing
    end
    basis
end

"""
    membership(goal, generators; budget = Budget(200_000))

Cofactors `q` with `goal = sum(q[i] * generators[i])`, or `nothing` when the
goal is not in the ideal of the generators, or `:budget` when the
computation was cut short.
"""
function membership(goal::Polynomial, generators::AbstractVector{Polynomial}; budget::Budget = Budget(200_000))
    iszero(goal) && return [zero(goal) for _ in generators]
    basis = groebner(generators; budget)
    basis === nothing && return :budget
    local quotients, remainder
    try
        quotients, remainder = divide(goal, [b.polynomial for b in basis], budget)
    catch e
        e isa BudgetSpent || rethrow()
        return :budget
    end
    iszero(remainder) || return nothing
    cofactors = [zero(goal) for _ in generators]
    for (k, q) in enumerate(quotients)
        iszero(q) && continue
        for g in eachindex(generators)
            cofactors[g] += q * basis[k].cofactors[g]
        end
    end
    cofactors
end
