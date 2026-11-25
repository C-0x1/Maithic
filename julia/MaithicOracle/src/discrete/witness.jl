# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Witnesses for existential goals over the integers, found by search.

"""
    WitnessCertificate(domain, variables, conditions, values)

There are integers (`domain = :int`) or natural numbers (`:nat`) satisfying
all the `conditions`, namely `values`, in the order of `variables`.
"""
struct WitnessCertificate <: Certificate
    domain::Symbol
    variables::Vector{String}
    conditions::Vector{Relation}
    values::Vector{BigInt}
end

kind(::WitnessCertificate) = "discrete.witness"

function claim(c::WitnessCertificate)
    conditions = join((relation_text(r; style = :lean) for r in c.conditions), " ∧ ")
    "∃ $(join(c.variables, " ")) : $(c.domain == :nat ? "ℕ" : "ℤ"), $(conditions)"
end

certificate_data(c::WitnessCertificate) =
    Object("domain" => String(c.domain), "variables" => c.variables,
           "conditions" => Any[relation_text(r) for r in c.conditions], "values" => Any[string(v) for v in c.values])

function check(c::WitnessCertificate)
    c.domain in (:int, :nat) || return false
    (length(c.values) == length(c.variables) && allunique(c.variables) && !isempty(c.conditions)) || return false
    c.domain == :nat && any(<(0), c.values) && return false
    issubset(variables(c.conditions), c.variables) || return false
    env = Dict{String,Exact}(name => Exact(value) for (name, value) in zip(c.variables, c.values))
    try
        all(holds(r, env) for r in c.conditions)
    catch e
        e isa Union{DivideError,InexactFunction} || rethrow()
        false
    end
end

# The conditions that mean the same over Lean's integers and naturals as in
# exact arithmetic: no division, no functions, and over the naturals no
# subtraction, which Lean truncates at zero.
lean_exact(n::Num, domain::Symbol) = denominator(n.value) == 1 && (domain == :int || n.value >= 0)
lean_exact(::Var, ::Symbol) = true
lean_exact(n::Neg, domain::Symbol) = domain == :int && lean_exact(n.arg, domain)
lean_exact(n::Pow, domain::Symbol) = lean_exact(n.base, domain)
lean_exact(::Call, ::Symbol) = false
lean_exact(n::BinOp, domain::Symbol) =
    n.op != :/ && (n.op != :- || domain == :int) && lean_exact(n.left, domain) && lean_exact(n.right, domain)
lean_exact(r::Relation, domain::Symbol) = lean_exact(r.left, domain) && lean_exact(r.right, domain)

function lean_statement(c::WitnessCertificate)
    all(r -> lean_exact(r, c.domain), c.conditions) || return nothing
    conditions = join((relation_text(r; style = :lean) for r in c.conditions), " ∧ ")
    string("example : ∃ ", join(c.variables, " "), " : ", c.domain == :nat ? "Nat" : "Int", ", ", conditions,
           " := ⟨", join(c.values, ", "), ", by decide⟩\n")
end

"""
Calls `visit` on every integer point whose largest coordinate, in absolute
value, is exactly `radius`, in lexicographic order; over the naturals only
the points without a negative coordinate. Stops and returns `true` as soon
as `visit` does.
"""
function visit_shell(visit, point::Vector{Int}, radius::Int, natural::Bool)
    dimension = length(point)
    dimension == 0 && return radius == 0 && visit(point)
    function fill(position::Int, on_shell::Bool)
        position > dimension && return visit(point)
        for value in (natural ? 0 : -radius):radius
            edge = abs(value) == radius
            position == dimension && !on_shell && !edge && continue
            point[position] = value
            fill(position + 1, on_shell || edge) && return true
        end
        false
    end
    fill(1, false)
end

"""
    find_witness(conditions; domain = :int, variables = nothing, bound = 64, budget = 500_000)

Searches for integers that satisfy all the conditions and returns a
[`WitnessCertificate`](@ref), or `nothing` when the search ends without one.
The conditions are comparisons, or one text with comparisons joined by `∧`.

Points are tried in order of their largest absolute coordinate, so the
witness found is a smallest one in that sense, and the order within one size
is fixed, so the same question always gets the same answer. The search stops
at `bound` or after `budget` points. Ending without a witness proves nothing
about the conditions.
"""
function find_witness(conditions::AbstractVector{Relation}; domain::Symbol = :int,
                      variables::Union{Nothing,AbstractVector{<:AbstractString}} = nothing, bound::Int = 64,
                      budget::Int = 500_000)
    domain in (:int, :nat) || throw(ArgumentError("witnesses are searched over :int or :nat"))
    isempty(conditions) && throw(ArgumentError("at least one condition is needed"))
    names = variables === nothing ? MaithicCore.variables(conditions) : String.(variables)
    issubset(MaithicCore.variables(conditions), names) || throw(ArgumentError("a condition uses a variable that is not listed"))
    env = Dict{String,Exact}(name => Exact(0) for name in names)
    point = zeros(Int, length(names))
    tried = 0
    function visit(candidate)
        tried += 1
        for (name, value) in zip(names, candidate)
            env[name] = Exact(value)
        end
        satisfied = try
            all(holds(r, env) for r in conditions)
        catch e
            e isa DivideError || rethrow()
            false
        end
        satisfied || tried >= budget
    end
    for radius in 0:bound
        if visit_shell(visit, point, radius, domain == :nat)
            certificate = WitnessCertificate(domain, names, collect(conditions), BigInt.(point))
            return check(certificate) ? certificate : nothing
        end
    end
    nothing
end

find_witness(conditions::AbstractString; options...) = find_witness(parse_conjunction(conditions); options...)

READERS["discrete.witness"] =
    d -> WitnessCertificate(Symbol(text_of(d["domain"])), String[text_of(v) for v in list_of(d["variables"])],
                            Relation[relation_of(r) for r in list_of(d["conditions"])],
                            BigInt[big_of(v) for v in list_of(d["values"])])
