# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The linear oracle: prove, refute, or say why neither.

"""
    is_counterexample(goal, model)

Whether the model satisfies every hypothesis of the goal and violates its
conclusion, in exact arithmetic. Every oracle runs this before it reports a
refutation.
"""
function is_counterexample(g::Goal, model)
    env = Dict{String,Exact}(name => value for (name, value) in model)
    try
        all(holds(h.statement, env) for h in g.hypotheses) && (g.goal === nothing || !holds(g.goal, env))
    catch e
        e isa Union{DivideError,InexactFunction,UnboundVariable} || rethrow()
        false
    end
end

"""
    prove_linear(goal)

Decides a goal of linear arithmetic.

- [`Proved`](@ref) with a [`LinearCertificate`](@ref): every case of the
  negated goal contradicts the hypotheses, with multipliers from the exact
  simplex method.
- [`Refuted`](@ref) with a model: a point that satisfies the hypotheses and
  violates the goal.
- [`Unknown`](@ref): a statement is not linear, or, over the integers, the
  rational relaxation has solutions but none was found among the integers.

Over the rationals the method is complete: it always proves or refutes. Over
the integers it is sound and incomplete; rows are tightened to their integer
content, which settles many goals, and a small search looks for an integer
counterexample before giving up.
"""
function prove_linear(g::Goal)
    domain = g.domain === :int ? :int : :rat
    width = length(g.variables)
    base = Row[]
    for h in g.hypotheses
        try
            push!(base, linear_row(h.statement, g.variables, domain))
        catch e
            e isa NotLinear || rethrow()
            return Unknown("hypothesis $(h.name) is $(sprint(showerror, e))")
        end
    end
    refutations = Refutation[]
    for branch in negation_branches(g.goal)
        rows = base
        if branch !== nothing
            try
                rows = vcat(base, linear_row(branch, g.variables, domain))
            catch e
                e isa NotLinear || rethrow()
                return Unknown("the goal is $(sprint(showerror, e))")
            end
        end
        isempty(rows) && return Refuted(Pair{String,Exact}[name => Exact(0) for name in g.variables])
        status, values = feasibility(rows, width)
        if status === :feasible
            model = Pair{String,Exact}[name => value for (name, value) in zip(g.variables, values)]
            if domain === :int && !all(isinteger, values)
                conditions = Relation[h.statement for h in g.hypotheses]
                branch === nothing || push!(conditions, branch)
                witness = find_witness(conditions; variables = g.variables, bound = 24, budget = 50_000)
                witness === nothing && return Unknown(
                    "the hypotheses and the negated goal have rational solutions, and no integer one was found nearby")
                model = Pair{String,Exact}[name => Exact(value) for (name, value) in zip(g.variables, witness.values)]
            end
            is_counterexample(g, model) || throw(ErrorException("the linear oracle produced a model that fails its check"))
            return Refuted(model)
        end
        push!(refutations, Refutation(branch, values))
    end
    certificate = LinearCertificate(domain, g.variables, g.hypotheses, g.goal, refutations)
    check(certificate) || throw(RejectedCertificate(kind(certificate)))
    Proved(certificate)
end

"""
    prove_linear(hypotheses, goal; domain = :rat, variables = nothing)

The same, from statements as text: `prove_linear(["x >= 1", "y >= x"], "y > 0")`.
"""
prove_linear(hypotheses::AbstractVector, goal; domain::Symbol = :rat, variables = nothing) =
    prove_linear(Goal(:linear, hypotheses, goal; domain, variables))
