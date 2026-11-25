# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Inequalities between real functions on a box, by enclosure and subdivision.

"A box: one closed interval with exact endpoints per variable, in the order of the variables."
const Bounds = Vector{Tuple{Exact,Exact}}

"""
    BoxClaim(bounds, reason, enclosure)

One box of an interval proof. With `reason = 0` the goal holds on the box,
because `enclosure` contains every value of the goal's `left - right` there
and lies on the right side of zero. With `reason = k > 0` no point of the box
satisfies hypothesis `k`, by the enclosure of that hypothesis's
`left - right`, so the box needs no proof.
"""
struct BoxClaim
    bounds::Bounds
    reason::Int
    enclosure::Tuple{Exact,Exact}
end

Base.:(==)(a::BoxClaim, b::BoxClaim) = a.bounds == b.bounds && a.reason == b.reason && a.enclosure == b.enclosure

"""
    IntervalCertificate(variables, hypotheses, goal, domain, boxes)

The goal holds wherever the hypotheses do. `domain` is the box that the
bound hypotheses (`0 <= x`, `x <= 1`, ...) confine the variables to, and
`boxes` cut it into pieces, each with a [`BoxClaim`](@ref).
"""
struct IntervalCertificate <: Certificate
    variables::Vector{String}
    hypotheses::Vector{Hypothesis}
    goal::Relation
    domain::Bounds
    boxes::Vector{BoxClaim}
end

kind(::IntervalCertificate) = "interval.boxes"

function claim(c::IntervalCertificate)
    steps = String[relation_text(h.statement; style = :lean) for h in c.hypotheses]
    push!(steps, relation_text(c.goal; style = :lean))
    isempty(c.variables) ? join(steps, " → ") : "∀ $(join(c.variables, " ")) : ℝ, $(join(steps, " → "))"
end

bounds_data(bounds) = Any[Any[exact_text(lo), exact_text(hi)] for (lo, hi) in bounds]

certificate_data(c::IntervalCertificate) =
    Object("variables" => c.variables,
           "hypotheses" => Any[Object("name" => h.name, "statement" => relation_text(h.statement)) for h in c.hypotheses],
           "goal" => relation_text(c.goal), "domain" => bounds_data(c.domain),
           "boxes" => Any[Object("bounds" => bounds_data(b.bounds), "reason" => b.reason,
                                 "enclosure" => Any[exact_text(b.enclosure[1]), exact_text(b.enclosure[2])]) for b in c.boxes])

"""
    variable_domain(hypotheses, variables)

The box implied by the hypotheses that bound a single variable by a
constant, such as `0 <= x`, `x < 1` or `2 * y = 3`. A strict bound is kept
as a closed one, which only enlarges the box. Returns the box and the
indices of the remaining hypotheses, or the name of a variable that is not
bounded on both sides.
"""
function variable_domain(hypotheses::AbstractVector{Hypothesis}, variables::AbstractVector{String})
    lower = Dict{String,Exact}()
    upper = Dict{String,Exact}()
    others = Int[]
    for (k, h) in enumerate(hypotheses)
        form = try
            h.statement.op === :ne ? nothing :
            added(linear_form(h.statement.left), scaled(linear_form(h.statement.right), Exact(-1)))
        catch e
            e isa NotLinear || rethrow()
            nothing
        end
        if form === nothing || length(form.coefficients) != 1
            push!(others, k)
            continue
        end
        name, coefficient = first(form.coefficients)
        bound = -form.constant / coefficient
        op = h.statement.op
        # form op 0 reads coefficient * name + constant op 0; dividing by a negative coefficient turns it around.
        is_upper = (op in (:le, :lt)) == (coefficient > 0)
        if op === :eq || is_upper
            upper[name] = min(get(upper, name, bound), bound)
        end
        if op === :eq || !is_upper
            lower[name] = max(get(lower, name, bound), bound)
        end
    end
    for name in variables
        (haskey(lower, name) && haskey(upper, name)) || return (name, others)
    end
    (Tuple{Exact,Exact}[(lower[name], upper[name]) for name in variables], others)
end

"Whether an enclosure of `left - right` shows that the comparison holds on the whole box."
function certainly_holds(op::Symbol, lo, hi)
    op === :le ? hi <= 0 : op === :lt ? hi < 0 : op === :ge ? lo >= 0 : op === :gt ? lo > 0 :
    op === :ne ? (hi < 0 || lo > 0) : (iszero(lo) && iszero(hi))
end

"Whether an enclosure of `left - right` shows that the comparison fails on the whole box."
certainly_fails(op::Symbol, lo, hi) = certainly_holds(NEGATION_OF[op], lo, hi)

const NEGATION_OF = Dict(:le => :gt, :gt => :le, :lt => :ge, :ge => :lt, :eq => :ne, :ne => :eq)

difference_enclosure(r::Relation, variables, bounds, ::Type{T}) where {T} =
    enclosure(BinOp(:-, r.left, r.right), Pair{String,Tuple{Exact,Exact}}[n => b for (n, b) in zip(variables, bounds)], T)

"""
    simple_enclosure(lo, hi, accept)

Rational numbers around `[lo, hi]` with as few decimal digits as possible
(3, 6, 9 or 12) such that `accept` still holds of them; the exact values of
the floating-point endpoints when no rounding survives. A certificate with
short numbers is easier to read and to check, and rounding outward never
makes an enclosure wrong.
"""
function simple_enclosure(lo::AbstractFloat, hi::AbstractFloat, accept)
    exact_lo, exact_hi = Exact(lo), Exact(hi)
    for digits in (3, 6, 9, 12)
        scale = big(10)^digits
        candidate = (floor(BigInt, exact_lo * scale) // scale, ceil(BigInt, exact_hi * scale) // scale)
        accept(candidate...) && return candidate
    end
    (exact_lo, exact_hi)
end

"""
    tiles(region, boxes)

Whether the boxes cut the region into pieces by repeated halving: either
there is one box and it is the region, or some coordinate's midpoint
separates the boxes into two groups that tile the two halves. Every way of
bisecting a region produces such a tiling, and a set of boxes that passes
covers the region.
"""
function tiles(region::Bounds, boxes::AbstractVector{BoxClaim})
    isempty(boxes) && return false
    length(boxes) == 1 && return boxes[1].bounds == region
    for d in eachindex(region)
        lo, hi = region[d]
        lo == hi && continue
        middle = (lo + hi) / 2
        all(b -> length(b.bounds) == length(region) && (b.bounds[d][2] <= middle || b.bounds[d][1] >= middle), boxes) || continue
        left = [b for b in boxes if b.bounds[d][2] <= middle]
        right = [b for b in boxes if b.bounds[d][2] > middle]
        (isempty(left) || isempty(right)) && continue
        halves = (copy(region), copy(region))
        halves[1][d] = (lo, middle)
        halves[2][d] = (middle, hi)
        return tiles(halves[1], left) && tiles(halves[2], right)
    end
    false
end

"""
    check(certificate::IntervalCertificate; T = BigFloat)

Checks an interval certificate with arithmetic that is independent of the
search: the domain is derived again from the hypotheses, the boxes must tile
it, and on every box the enclosure is computed again, in `BigFloat` where
the search used `Float64`, and must lie inside the claimed one, which in
turn must decide the box.
"""
function check(c::IntervalCertificate; T::Type{<:AbstractFloat} = BigFloat)
    (allunique(c.variables) && allunique(h.name for h in c.hypotheses)) || return false
    c.goal.op === :eq && return false
    try
        domain, _ = variable_domain(c.hypotheses, c.variables)
        (domain isa Bounds && domain == c.domain && all(lo <= hi for (lo, hi) in domain)) || return false
        tiles(c.domain, c.boxes) || return false
        for box in c.boxes
            (0 <= box.reason <= length(c.hypotheses) && box.enclosure[1] <= box.enclosure[2]) || return false
            statement = box.reason == 0 ? c.goal : c.hypotheses[box.reason].statement
            computed = difference_enclosure(statement, c.variables, box.bounds, T)
            (isfinite(computed.lo) && isfinite(computed.hi)) || return false
            (box.enclosure[1] <= Exact(computed.lo) && Exact(computed.hi) <= box.enclosure[2]) || return false
            decided = box.reason == 0 ? certainly_holds(statement.op, box.enclosure...) :
                      certainly_fails(statement.op, box.enclosure...)
            decided || return false
        end
        true
    catch e
        e isa Union{Undefined,UnboundVariable,ArgumentError,DivideError} || rethrow()
        false
    end
end

function interval_certificate_of(d)
    pair_of(value) = begin
        items = list_of(value)
        length(items) == 2 || throw(ArgumentError("an interval has two endpoints"))
        (exact_of(items[1]), exact_of(items[2]))
    end
    bounds_of(value) = Tuple{Exact,Exact}[pair_of(v) for v in list_of(value)]
    IntervalCertificate(String[text_of(v) for v in list_of(d["variables"])],
                        Hypothesis[hypothesis_of(h) for h in list_of(d["hypotheses"])], relation_of(d["goal"]),
                        bounds_of(d["domain"]),
                        BoxClaim[BoxClaim(bounds_of(b["bounds"]), count_of(b["reason"]), pair_of(b["enclosure"]))
                                 for b in list_of(d["boxes"])])
end

READERS["interval.boxes"] = interval_certificate_of

"What the search concluded about one box."
function decide_box(g::Goal, others::Vector{Int}, bounds::Bounds)
    for k in others
        statement = g.hypotheses[k].statement
        e = try
            difference_enclosure(statement, g.variables, bounds, Float64)
        catch err
            err isa Undefined || rethrow()
            continue
        end
        if isfinite(e.lo) && isfinite(e.hi) && certainly_fails(statement.op, e.lo, e.hi)
            return BoxClaim(bounds, k, simple_enclosure(e.lo, e.hi, (lo, hi) -> certainly_fails(statement.op, lo, hi)))
        end
    end
    e = try
        difference_enclosure(g.goal, g.variables, bounds, Float64)
    catch err
        err isa Undefined || rethrow()
        return nothing
    end
    (isfinite(e.lo) && isfinite(e.hi) && certainly_holds(g.goal.op, e.lo, e.hi)) || return nothing
    BoxClaim(bounds, 0, simple_enclosure(e.lo, e.hi, (lo, hi) -> certainly_holds(g.goal.op, lo, hi)))
end

"""
    refutes_at(goal, point, T = BigFloat)

Whether, at the point, every hypothesis certainly holds and the goal
certainly fails, by enclosures of the values there. `true` is a proof that
the point is a counterexample; `false` proves nothing.
"""
function refutes_at(g::Goal, point::Vector{Exact}, ::Type{T} = BigFloat) where {T<:AbstractFloat}
    bounds = Tuple{Exact,Exact}[(x, x) for x in point]
    try
        e = difference_enclosure(g.goal, g.variables, bounds, T)
        certainly_fails(g.goal.op, e.lo, e.hi) || return false
        for h in g.hypotheses
            e = difference_enclosure(h.statement, g.variables, bounds, T)
            certainly_holds(h.statement.op, e.lo, e.hi) || return false
        end
        true
    catch err
        err isa Undefined || rethrow()
        false
    end
end

"The centre of a box and, in up to six dimensions, its corners: the points at which a counterexample is looked for."
function probe_points(bounds::Bounds)
    points = Vector{Exact}[Exact[(lo + hi) / 2 for (lo, hi) in bounds]]
    dimension = length(bounds)
    if 1 <= dimension <= 6
        for corner in 0:(2^dimension - 1)
            push!(points, Exact[bounds[d][((corner >> (d - 1)) & 1) + 1] for d in 1:dimension])
        end
    end
    points
end

"""
    prove_real(goal; max_boxes = 20_000)

Decides an inequality between real functions on a bounded box.

The hypotheses of the form `c <= x` and `x <= c` fix the box. On a box the
goal's `left - right` is enclosed by interval arithmetic; when the enclosure
decides the goal the box is done, when it shows that another hypothesis
fails on the whole box the box is discarded, and otherwise a counterexample
is looked for at the centre and the corners of the box, and the box is cut
in half across its longest side. Boxes are taken in the order they were
made, the large ones first, so that a counterexample far from the first
corner is not starved by an endless descent elsewhere.

- [`Proved`](@ref) with an [`IntervalCertificate`](@ref) when every box is
  decided, after the certificate has passed its second check in `BigFloat`.
- [`Refuted`](@ref) with a rational point at which the hypotheses certainly
  hold and the goal certainly fails, found in `Float64` and confirmed in
  `BigFloat`.
- [`Unknown`](@ref) when a variable is unbounded, when the goal is an
  equation, or when `max_boxes` boxes did not settle it. A goal that holds
  with equality somewhere in the interior, such as `x^2 - 2*x + 1 >= 0`,
  cannot be settled by enclosures alone.
"""
function prove_real(g::Goal; max_boxes::Int = 20_000)
    g.goal === nothing && return Unknown("the interval oracle proves inequalities, not contradictions")
    g.goal.op === :eq && return Unknown("an equation cannot be proved by enclosures")
    domain, others = variable_domain(g.hypotheses, g.variables)
    domain isa Bounds || return Unknown("the variable $(domain) is not bounded on both sides by the hypotheses")
    for (name, (lo, hi)) in zip(g.variables, domain)
        lo <= hi || return Unknown("the bounds on $(name) admit no value, so the goal holds for want of a case; " *
                                   "the linear oracle certifies that")
    end
    queue = Bounds[domain]
    done = BoxClaim[]
    examined = 0
    while examined < length(queue)
        examined == max_boxes &&
            return Unknown("undecided after $(max_boxes) boxes; the goal may hold with equality somewhere, or need a finer method")
        examined += 1
        bounds = queue[examined]
        claimed = decide_box(g, others, bounds)
        if claimed !== nothing
            push!(done, claimed)
            continue
        end
        for point in probe_points(bounds)
            if refutes_at(g, point, Float64) && refutes_at(g, point, BigFloat)
                return Refuted(Pair{String,Exact}[n => x for (n, x) in zip(g.variables, point)])
            end
        end
        widths = [hi - lo for (lo, hi) in bounds]
        all(iszero, widths) && return Unknown("undecided at the single point of the domain")
        d = argmax(widths)
        lo, hi = bounds[d]
        lower_half, upper_half = copy(bounds), copy(bounds)
        lower_half[d] = (lo, (lo + hi) / 2)
        upper_half[d] = ((lo + hi) / 2, hi)
        push!(queue, lower_half, upper_half)
    end
    certificate = IntervalCertificate(g.variables, g.hypotheses, g.goal, domain, done)
    check(certificate) || throw(RejectedCertificate(kind(certificate)))
    Proved(certificate)
end

"""
    prove_real(hypotheses, goal; variables = nothing, max_boxes = 20_000)

The same, from statements as text:
`prove_real(["0 <= x", "x <= 1"], "x * (1 - x) <= 1/4 + 1/100")`.
"""
prove_real(hypotheses::AbstractVector, goal; variables = nothing, max_boxes::Int = 20_000) =
    prove_real(Goal(:real, hypotheses, goal; domain = :real, variables); max_boxes)

"""
    solve(goal; options...)

Sends a goal, or the text of a goal document, to the oracle for its kind:
[`prove_linear`](@ref), [`prove_polynomial`](@ref) or [`prove_real`](@ref).
The options are those of the oracle chosen, such as `steps` or `max_boxes`.
"""
solve(g::Goal; options...) =
    g.kind === :linear ? prove_linear(g) : g.kind === :polynomial ? prove_polynomial(g; options...) : prove_real(g; options...)
solve(text::AbstractString; options...) = solve(read_goal(text); options...)
