# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Certificates of linear arithmetic: multipliers that add the hypotheses up to
# a contradiction.

"""
    negation_branches(goal)

The cases in which a goal fails, each as one comparison: one case for an
inequality or a `!=`, two for an equation (`a = b` fails when `a < b` or
when `a > b`). For the goal `false` there is one case that assumes nothing.
"""
negation_branches(::Nothing) = Union{Nothing,Relation}[nothing]
negation_branches(goal::Relation) =
    goal.op === :eq ? Union{Nothing,Relation}[Relation(:lt, goal.left, goal.right), Relation(:gt, goal.left, goal.right)] :
    Union{Nothing,Relation}[negate(goal)]

"""
    Refutation(assumption, multipliers)

One case of a linear proof: the hypotheses together with `assumption` (a
case of the negated goal, or nothing) are contradictory, and `multipliers`,
one per hypothesis and then one for the assumption, show it by
[`refutes`](@ref).
"""
struct Refutation
    assumption::Union{Nothing,Relation}
    multipliers::Vector{Exact}
end

Base.:(==)(a::Refutation, b::Refutation) = a.assumption == b.assumption && a.multipliers == b.multipliers

"""
    LinearCertificate(domain, variables, hypotheses, goal, refutations)

The hypotheses imply the goal over the rationals (`domain = :rat`) or the
integers (`:int`): each case of the negated goal has a
[`Refutation`](@ref). With the goal `nothing` the certificate says that the
hypotheses contradict each other, and its single refutation is a Farkas
certificate in the plain sense.

Over the integers every row is first tightened by [`tighten`](@ref); the
check does the same, so the multipliers refer to the tightened rows.
"""
struct LinearCertificate <: Certificate
    domain::Symbol
    variables::Vector{String}
    hypotheses::Vector{Hypothesis}
    goal::Union{Nothing,Relation}
    refutations::Vector{Refutation}
end

kind(c::LinearCertificate) = c.goal === nothing ? "linear.farkas" : "linear.combination"

function claim(c::LinearCertificate)
    steps = String[relation_text(h.statement; style = :lean) for h in c.hypotheses]
    push!(steps, c.goal === nothing ? "False" : relation_text(c.goal; style = :lean))
    body = join(steps, " → ")
    isempty(c.variables) ? body : "∀ $(join(c.variables, " ")) : $(c.domain === :int ? "ℤ" : "ℚ"), $(body)"
end

certificate_data(c::LinearCertificate) =
    Object("domain" => String(c.domain), "variables" => c.variables,
           "hypotheses" => Any[Object("name" => h.name, "statement" => relation_text(h.statement)) for h in c.hypotheses],
           "goal" => c.goal === nothing ? "false" : relation_text(c.goal),
           "refutations" => Any[Object("assumption" => r.assumption === nothing ? nothing : relation_text(r.assumption),
                                       "multipliers" => Any[exact_text(m) for m in r.multipliers])
                                for r in c.refutations])

function check(c::LinearCertificate)
    c.domain in (:rat, :int) || return false
    (allunique(c.variables) && allunique(h.name for h in c.hypotheses)) || return false
    branches = negation_branches(c.goal)
    length(c.refutations) == length(branches) || return false
    try
        base = Row[linear_row(h.statement, c.variables, c.domain) for h in c.hypotheses]
        for (branch, refutation) in zip(branches, c.refutations)
            refutation.assumption == branch || return false
            rows = branch === nothing ? base : vcat(base, linear_row(branch, c.variables, c.domain))
            refutes(rows, refutation.multipliers) || return false
        end
        true
    catch e
        e isa Union{NotLinear,ArgumentError} || rethrow()
        false
    end
end

# A name that Lean reads as one identifier: letters of any alphabet, digits, subscripts, `_`, `'`, `!` and `?`.
lean_name(name::AbstractString) = occursin(r"^[\p{L}_][\p{L}\p{N}_'!?]*$", name) ? name : "_"
has_power(n::Pow) = true
has_power(n::Union{Num,Var}) = false
has_power(n::Neg) = has_power(n.arg)
has_power(n::BinOp) = has_power(n.left) || has_power(n.right)
has_power(n::Call) = any(has_power, n.args)
has_power(r::Relation) = has_power(r.left) || has_power(r.right)

"A comparison as a Lean proposition whose left side fixes the number type."
typed_relation(r::Relation, type::String) =
    string("(", expression_text(r.left; style = :lean), " : ", type, ") ",
           MaithicCore.LEAN_RELATIONS[r.op], " ", expression_text(r.right; style = :lean))

"""
    lean_statement(certificate::LinearCertificate)

The claim as a Lean `example` closed by a decision procedure of core Lean:
`omega` over the integers, `grind` over the rationals. It is an independent
confirmation of the claim by the kernel; it does not replay the multipliers,
which is the task of the certificate checker. Over the integers the text is
`nothing` when a statement contains a division or a power, whose meaning in
Lean's integers differs or which `omega` does not read.
"""
function lean_statement(c::LinearCertificate)
    statements = Relation[h.statement for h in c.hypotheses]
    c.goal === nothing || push!(statements, c.goal)
    if c.domain === :int
        all(r -> lean_exact(r, :int) && !has_power(r), statements) || return nothing
    end
    type = c.domain === :int ? "Int" : "Rat"
    binders = isempty(c.variables) ? "" : " ($(join(c.variables, " ")) : $(type))"
    hypotheses = join(" ($(lean_name(h.name)) : $(typed_relation(h.statement, type)))" for h in c.hypotheses)
    goal = c.goal === nothing ? "False" : typed_relation(c.goal, type)
    "example$(binders)$(hypotheses) : $(goal) := by $(c.domain === :int ? "omega" : "grind")\n"
end

"A name for the negated goal that no variable or hypothesis has taken."
function fresh_name(taken)
    name = "negated"
    while name in taken
        name *= "'"
    end
    name
end

"""
    lean_replay(certificate::LinearCertificate; name = nothing)

The claim as a Lean theorem closed by `maithic_linear_int`, for certificates
over the integers. Every case of the negated goal becomes one call: the rows
are the tightened rows of the check, the multipliers are scaled to whole
numbers, rows with multiplier zero are left out, and every row names the
hypothesis it comes from. Lean then confirms two things separately: the
kernel runs the verified checker on rows and multipliers, and `omega`
derives each row from its one hypothesis.

The result is `nothing` over the rationals, and when a statement contains a
division or a power.
"""
function lean_replay(c::LinearCertificate; name::Union{Nothing,AbstractString} = nothing)
    c.domain === :int || return nothing
    statements = Relation[h.statement for h in c.hypotheses]
    c.goal === nothing || push!(statements, c.goal)
    all(r -> lean_exact(r, :int) && !has_power(r), statements) || return nothing
    check(c) || throw(RejectedCertificate(kind(c)))
    name === nothing || occursin(r"^[A-Za-z_][A-Za-z0-9_]*$", name) ||
        throw(ArgumentError("\"$(name)\" is not a name Lean accepts"))
    names = String[lean_name(h.name) == "_" ? "hypothesis_$(i)" : h.name for (i, h) in enumerate(c.hypotheses)]
    negated = fresh_name(vcat(names, c.variables))
    base = Row[linear_row(h.statement, c.variables, :int) for h in c.hypotheses]
    whole(x) = string(numerator(x))
    function call(refutation::Refutation, indent::String)
        rows, sources = base, names
        if refutation.assumption !== nothing
            rows = vcat(base, linear_row(refutation.assumption, c.variables, :int))
            sources = vcat(names, negated)
        end
        scale = foldl(lcm, (denominator(m) for m in refutation.multipliers); init = big(1))
        entries, used = String[], String[]
        for (row, multiplier, source) in zip(rows, refutation.multipliers, sources)
            iszero(multiplier) && continue
            push!(entries, "($(whole(multiplier * scale)), ⟨[$(join(whole.(row.coefficients), ", "))], .$(row.op), $(whole(row.bound))⟩)")
            push!(used, source)
        end
        "$(indent)maithic_linear_int [$(join(c.variables, ", "))] [$(join(entries, ", "))] using [$(join(used, ", "))]"
    end
    binders = isempty(c.variables) ? "" : " ($(join(c.variables, " ")) : Int)"
    hypotheses = join(" ($(n) : $(typed_relation(h.statement, "Int")))" for (n, h) in zip(names, c.hypotheses))
    goal = c.goal === nothing ? "False" : typed_relation(c.goal, "Int")
    lines = String["$(name === nothing ? "example" : "theorem $(name)")$(binders)$(hypotheses) : $(goal) := by"]
    if c.goal === nothing
        push!(lines, call(c.refutations[1], "  "))
    elseif c.goal.op === :ne
        push!(lines, "  intro $(negated)", call(c.refutations[1], "  "))
    elseif c.goal.op === :eq
        push!(lines, "  apply Classical.byContradiction", "  intro $(negated)",
              "  rcases Int.lt_or_gt_of_ne $(negated) with $(negated) | $(negated)",
              call(c.refutations[1], "  · "), call(c.refutations[2], "  · "))
    else
        push!(lines, "  apply Classical.byContradiction", "  intro $(negated)", call(c.refutations[1], "  "))
    end
    join(lines, "\n") * "\n"
end

function lean_scripts(c::LinearCertificate)
    scripts = String[]
    replay = lean_replay(c)
    replay === nothing || push!(scripts, tactic_block(replay))
    check = lean_statement(c)
    check === nothing || push!(scripts, c.domain === :int ? "omega" : "grind")
    scripts
end

function hypothesis_of(entry)
    Hypothesis(text_of(entry["name"]), relation_of(entry["statement"]))
end

function linear_certificate_of(d)
    goal = text_of(d["goal"])
    refutations = Refutation[
        Refutation(r["assumption"] === nothing ? nothing : relation_of(r["assumption"]),
                   Exact[exact_of(m) for m in list_of(r["multipliers"])]) for r in list_of(d["refutations"])]
    LinearCertificate(Symbol(text_of(d["domain"])), String[text_of(v) for v in list_of(d["variables"])],
                      Hypothesis[hypothesis_of(h) for h in list_of(d["hypotheses"])],
                      goal == "false" ? nothing : parse_relation(goal), refutations)
end

READERS["linear.farkas"] = linear_certificate_of
READERS["linear.combination"] = linear_certificate_of
