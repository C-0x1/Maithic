# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Goals in the oracle exchange format.

"A named hypothesis of a goal."
struct Hypothesis
    name::String
    statement::Relation
end

"""
    Goal

A goal in the oracle exchange format, schema `maithic.oracle.goal/1`:

```json
{
  "schema": "maithic.oracle.goal/1",
  "kind": "linear",
  "domain": "rat",
  "variables": ["x", "y"],
  "hypotheses": [{"name": "h1", "statement": "2*x + 3*y <= 6"}, "x >= 0"],
  "goal": "x + y <= 3"
}
```

`kind` is `linear`, `polynomial` or `real`; `domain` is `int`, `rat` or
`real`. A hypothesis is an object with a name or just its statement, in
which case it is named `h1`, `h2`, … by position. The goal is a comparison,
or `"false"` to ask for a refutation of the hypotheses. `variables` may be
omitted; it is then the sorted list of the names that occur.
"""
struct Goal
    kind::Symbol
    domain::Symbol
    variables::Vector{String}
    hypotheses::Vector{Hypothesis}
    goal::Union{Nothing,Relation}
end

const GOAL_SCHEMA = "maithic.oracle.goal/1"
const GOAL_KINDS = (:linear, :polynomial, :real)
const GOAL_DOMAINS = (:int, :rat, :real)

function Goal(kind::Symbol, hypotheses::AbstractVector, goal; domain::Symbol = kind == :real ? :real : :rat,
              variables::Union{Nothing,AbstractVector{<:AbstractString}} = nothing)
    kind in GOAL_KINDS || throw(ArgumentError("unknown goal kind \"$(kind)\""))
    domain in GOAL_DOMAINS || throw(ArgumentError("unknown domain \"$(domain)\""))
    named = Hypothesis[]
    for (i, h) in enumerate(hypotheses)
        push!(named, h isa Hypothesis ? h : Hypothesis("h$(i)", h isa Relation ? h : parse_relation(h)))
    end
    allunique(h.name for h in named) || throw(ArgumentError("two hypotheses share a name"))
    target = goal === nothing || goal isa Relation ? goal : goal == "false" ? nothing : parse_relation(goal)
    statements = Any[h.statement for h in named]
    target === nothing || push!(statements, target)
    used = MaithicCore.variables(statements)
    names = variables === nothing ? used : String.(variables)
    allunique(names) || throw(ArgumentError("a variable is listed twice"))
    missing_names = setdiff(used, names)
    isempty(missing_names) || throw(ArgumentError("the variable \"$(first(missing_names))\" is used but not listed"))
    Goal(kind, domain, names, named, target)
end

"""
    read_goal(text)

Reads a goal document. Anything that does not follow the schema is an
`ArgumentError` that says what is wrong; a statement that cannot be parsed
is reported with the name of its hypothesis.
"""
read_goal(text::AbstractString) = read_goal(read_json(text))

function read_goal(doc)
    expect_schema(doc, GOAL_SCHEMA)
    kind = Symbol(text_of(get(doc, "kind", nothing)))
    domain = haskey(doc, "domain") ? Symbol(text_of(doc["domain"])) : kind == :real ? :real : :rat
    hypotheses = Hypothesis[]
    for (i, entry) in enumerate(list_of(get(doc, "hypotheses", Any[])))
        name = entry isa Object ? text_of(get(entry, "name", "h$(i)")) : "h$(i)"
        statement = entry isa Object ? get(entry, "statement", nothing) : entry
        statement isa AbstractString || throw(ArgumentError("hypothesis $(name) has no statement"))
        relation = try
            parse_relation(statement)
        catch e
            e isa ParseError || rethrow()
            throw(ArgumentError("hypothesis $(name): $(sprint(showerror, e))"))
        end
        push!(hypotheses, Hypothesis(name, relation))
    end
    target = get(doc, "goal", nothing)
    target isa AbstractString || throw(ArgumentError("the goal must be a comparison or \"false\""))
    goal = try
        target == "false" ? nothing : parse_relation(target)
    catch e
        e isa ParseError || rethrow()
        throw(ArgumentError("goal: $(sprint(showerror, e))"))
    end
    variables = haskey(doc, "variables") ? String[text_of(v) for v in list_of(doc["variables"])] : nothing
    Goal(kind, hypotheses, goal; domain, variables)
end

"The goal as a document of the schema `maithic.oracle.goal/1`."
goal_document(g::Goal) =
    document(GOAL_SCHEMA, "kind" => String(g.kind), "domain" => String(g.domain), "variables" => g.variables,
             "hypotheses" => Any[Object("name" => h.name, "statement" => relation_text(h.statement))
                                 for h in g.hypotheses],
             "goal" => g.goal === nothing ? "false" : relation_text(g.goal))

Base.:(==)(a::Hypothesis, b::Hypothesis) = a.name == b.name && a.statement == b.statement
Base.:(==)(a::Goal, b::Goal) = a.kind == b.kind && a.domain == b.domain && a.variables == b.variables &&
                               a.hypotheses == b.hypotheses && a.goal == b.goal
