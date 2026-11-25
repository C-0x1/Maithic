# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Certificates and outcomes: what every oracle returns.

"""
    Certificate

A piece of data from which a fact can be checked without trusting whoever
produced the data. Each kind of certificate defines

- [`kind`](@ref): its name in the certificate schema, such as `"linear.farkas"`;
- [`claim`](@ref): the fact, as a line of text in Lean's notation;
- [`check`](@ref): the arithmetic that establishes the fact from the data;
- `certificate_data`: the data as a JSON object, numbers as exact text.
"""
abstract type Certificate end

"The name of a certificate's kind in the schema `maithic.oracle.certificate/1`."
function kind end

"The fact a certificate establishes, as one line of text in Lean's notation."
function claim end

"""
    check(certificate)

Whether the certificate establishes its claim. The check uses exact
arithmetic and nothing of the search that found the certificate, so it also
judges certificates that were read from a file. It never throws on bad data;
it returns `false`.
"""
function check end

function certificate_data end

"""
    RejectedCertificate(kind)

A certificate that fails its own check was about to be written. This is a
defect in the oracle that produced it, and nothing is written.
"""
struct RejectedCertificate <: Exception
    kind::String
end

Base.showerror(io::IO, e::RejectedCertificate) =
    print(io, "a certificate of kind \"", e.kind, "\" fails its own check and was not written")

const CERTIFICATE_SCHEMA = "maithic.oracle.certificate/1"

"""
    certificate_document(certificate, parameters...)

The certificate in the schema `maithic.oracle.certificate/1`: its kind, its
claim, its data, and a record of the engine with the given parameters. The
certificate is checked first; one that fails is a
[`RejectedCertificate`](@ref) error.
"""
function certificate_document(certificate::Certificate, parameters::Pair...)
    check(certificate) || throw(RejectedCertificate(kind(certificate)))
    document(CERTIFICATE_SCHEMA,
             "kind" => kind(certificate),
             "claim" => claim(certificate),
             "data" => certificate_data(certificate),
             "engine" => engine_record("MaithicOracle", VERSION, parameters...))
end

"The JSON text of [`certificate_document`](@ref)."
certificate_json(certificate::Certificate, parameters::Pair...; pretty::Bool = false) =
    write_json(certificate_document(certificate, parameters...); pretty)

"""
    lean_check(certificate; name = nothing)

Lean source that restates the arithmetic of the certificate for the Lean
kernel, closed by a decision procedure of core Lean (`decide`, `omega` or
`grind`), for the kinds whose arithmetic core Lean can evaluate; `nothing`
for the kinds that need a checker from the Mathlib tier. The text imports
nothing. It is an `example`, or a `theorem` of the given name.
"""
function lean_check(certificate::Certificate; name::Union{Nothing,AbstractString} = nothing)
    text = lean_statement(certificate)
    (text === nothing || name === nothing) && return text
    occursin(r"^[A-Za-z_][A-Za-z0-9_]*$", name) || throw(ArgumentError("\"$(name)\" is not a name Lean accepts"))
    replace(text, r"^example" => "theorem $(name)"; count = 1)
end

"The statement behind [`lean_check`](@ref) for one kind of certificate, as an `example`."
lean_statement(::Certificate) = nothing

"""
    lean_replay(certificate; name = nothing)

Lean source that proves the claim of the certificate from its data, by a
checker of the Maithic Lean library whose soundness is proved there;
`nothing` for the kinds and domains that have no such checker yet. Where
[`lean_check`](@ref) asks a decision procedure of Lean to confirm the claim
on its own, the replay uses what the oracle found: for linear arithmetic
over the integers the kernel runs the checker on the rows and multipliers.

The text is an `example`, or a `theorem` of the given name, and needs the
import [`LEAN_REPLAY_IMPORT`](@ref). A certificate that fails its own check
is a [`RejectedCertificate`](@ref) error.
"""
lean_replay(::Certificate; name::Union{Nothing,AbstractString} = nothing) = nothing

"The import line a file of [`lean_replay`](@ref) texts starts with."
const LEAN_REPLAY_IMPORT = "import Maithic.Oracle.Logic.LinearIntTactic"

"What an oracle found out about a goal."
abstract type Outcome end

"The goal holds, and here is the certificate."
struct Proved <: Outcome
    certificate::Certificate
end

"""
The goal fails at `model`, an assignment of exact values to the variables
that satisfies every hypothesis and violates the goal.
"""
struct Refuted <: Outcome
    model::Vector{Pair{String,Exact}}
end

"The oracle could not decide, for the stated reason. It is never a verdict on the goal."
struct Unknown <: Outcome
    reason::String
end

Base.:(==)(a::Refuted, b::Refuted) = a.model == b.model
Base.:(==)(a::Unknown, b::Unknown) = a.reason == b.reason

Base.show(io::IO, o::Proved) = print(io, "Proved(", kind(o.certificate), ": ", claim(o.certificate), ")")
Base.show(io::IO, o::Refuted) =
    print(io, "Refuted(", join(("$(name) = $(exact_text(value))" for (name, value) in o.model), ", "), ")")
Base.show(io::IO, o::Unknown) = print(io, "Unknown(", repr(o.reason), ")")

const OUTCOME_SCHEMA = "maithic.oracle.outcome/1"

model_object(model) = Object((name => exact_text(value) for (name, value) in model)...)

"""
    lean_scripts(certificate)

Tactic scripts that close the goal of the certificate inside the theorem it
was exported from, the most informative first: the replay of the certificate
through its verified checker, where there is one, and then the decision
procedure of Lean that confirms the claim on its own. The hypotheses are
referred to by the names the goal gave them. The list is empty for kinds
that Lean cannot check yet.
"""
lean_scripts(::Certificate) = String[]

"The tactic block of a theorem text: what follows `:= by`, without its indentation."
function tactic_block(text::AbstractString)
    head, body = split(text, ":= by"; limit = 2)
    lines = split(strip(body, ['\n', ' ']), '\n')
    join((startswith(line, "  ") ? line[3:end] : lstrip(line) for line in lines), "\n")
end

"""
    outcome_document(outcome; lean = false)

An outcome in the schema `maithic.oracle.outcome/1`: `"proved"` with the
certificate document, `"refuted"` with the model, or `"unknown"` with the
reason. With `lean = true` a proved outcome also carries what Lean needs to
check it: the statement of [`lean_check`](@ref), the theorem of
[`lean_replay`](@ref), and the scripts of [`lean_scripts`](@ref).
"""
function outcome_document(o::Proved; lean::Bool = false)
    members = Pair{String,Any}["outcome" => "proved", "certificate" => certificate_document(o.certificate)]
    lean && push!(members, "lean" => Object("check" => lean_check(o.certificate), "replay" => lean_replay(o.certificate),
                                            "scripts" => Any[s for s in lean_scripts(o.certificate)]))
    document(OUTCOME_SCHEMA, members...)
end
outcome_document(o::Refuted; lean::Bool = false) = document(OUTCOME_SCHEMA, "outcome" => "refuted", "model" => model_object(o.model))
outcome_document(o::Unknown; lean::Bool = false) = document(OUTCOME_SCHEMA, "outcome" => "unknown", "reason" => o.reason)

"""
    answer(goal_text; pretty = true)

The outcome of [`solve`](@ref) for a goal document, as JSON text with the
members for Lean. This is what the `maithic` command reads when it sends a
goal to an oracle. A goal that cannot be read is an `ArgumentError`.
"""
answer(goal_text::AbstractString; pretty::Bool = true) = write_json(outcome_document(solve(goal_text); lean = true); pretty)

# Reading helpers shared by the certificate kinds. They throw on malformed data;
# `read_certificate` turns that into an ArgumentError.
big_of(text) = text isa AbstractString ? parse(BigInt, text) : throw(ArgumentError("an integer must be written as text"))
exact_of(text) = text isa AbstractString ? parse_exact(text) : throw(ArgumentError("a number must be written as text"))
count_of(value) = value isa Integer && !(value isa Bool) ? Int(value) : throw(ArgumentError("a count must be an integer"))
text_of(value) = value isa AbstractString ? String(value) : throw(ArgumentError("text is expected"))
list_of(value) = value isa AbstractVector ? value : throw(ArgumentError("a list is expected"))
relation_of(text) = parse_relation(text_of(text))
