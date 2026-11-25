# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Tactics with their arguments taken out.
#
# `simp [double]` and `simp [List.length_append]` are the same move with
# different lemmas. A model that predicts whole tactics would have to guess
# the lemma; a model that predicts the move can be right, and the lemma is
# filled in by whoever proposes the candidates.

"The class of every tactic whose template the vocabulary does not hold."
const OTHER = "<other>"

# Words that stay in a template, because they change what the tactic does.
const KEPT_WORDS = Set(["at", "with", "using", "only", "generalizing", "in"])

"""
    template(tactic)

A tactic with its arguments replaced by slots: the name of the tactic stays,
a list of lemmas in square brackets becomes `[_]`, the words `at`, `with`,
`using`, `only`, `generalizing` and `in` stay, and everything else between
them becomes `_`.

    template("simp only [foo, bar] at h")       == "simp only [_] at _"
    template("exact h.trans (le_of_lt hk)")     == "exact _"
    template("omega")                           == "omega"
"""
function template(tactic::AbstractString)
    tokens = lex(tactic)
    isempty(tokens) && return ""
    parts = String[tokens[1].text]
    i = 2
    slot() = (parts[end] == "_" || push!(parts, "_"); nothing)
    while i <= length(tokens)
        token = tokens[i]
        if token.kind === :punctuation && token.text == "["
            depth = 1
            i += 1
            while i <= length(tokens) && depth > 0
                tokens[i].text == "[" && (depth += 1)
                tokens[i].text == "]" && (depth -= 1)
                i += 1
            end
            push!(parts, "[_]")
        elseif token.kind === :punctuation && token.text in ("(", "{", "⟨")
            closing = token.text == "(" ? ")" : token.text == "{" ? "}" : "⟩"
            depth = 1
            i += 1
            while i <= length(tokens) && depth > 0
                tokens[i].text == token.text && (depth += 1)
                tokens[i].text == closing && (depth -= 1)
                i += 1
            end
            slot()
        elseif token.kind === :identifier && token.text in KEPT_WORDS
            push!(parts, token.text)
            i += 1
        else
            slot()
            i += 1
        end
    end
    join(parts, " ")
end

"""
    TemplateVocabulary

The classes of the tactic classifier: the templates seen often enough, with
how often each was seen and how often its tactic closed a goal, and before
them the class [`OTHER`](@ref) for every other tactic.
"""
struct TemplateVocabulary
    templates::Vector{String}
    counts::Vector{Int}
    closes::Vector{Float64}
    index::Dict{String,Int}
end

function TemplateVocabulary(templates::Vector{String}, counts::Vector{Int}, closes::Vector{Float64})
    length(templates) == length(counts) == length(closes) || throw(ArgumentError("a template needs a count and a closing rate"))
    !isempty(templates) && templates[1] == OTHER || throw(ArgumentError("the first class is $(OTHER)"))
    TemplateVocabulary(templates, counts, closes, Dict(t => i for (i, t) in enumerate(templates)))
end

Base.length(v::TemplateVocabulary) = length(v.templates)
Base.:(==)(a::TemplateVocabulary, b::TemplateVocabulary) = a.templates == b.templates && a.counts == b.counts && a.closes == b.closes

"The class of a tactic: that of its template, or that of [`OTHER`](@ref)."
class_of(v::TemplateVocabulary, tactic::AbstractString) = get(v.index, template(tactic), 1)

"""
    build_templates(examples; min_count = 2)

The template vocabulary of a set of examples, each with a `tactic` and a
flag `closes` that says whether fewer goals stood after the tactic than
before. Templates seen at least `min_count` times become classes, the most
frequent first; the rest are counted under [`OTHER`](@ref).
"""
function build_templates(examples; min_count::Integer = 2)
    seen = Dict{String,Tuple{Int,Int}}()
    for example in examples
        key = template(example.tactic)
        uses, closed = get(seen, key, (0, 0))
        seen[key] = (uses + 1, closed + (example.closes ? 1 : 0))
    end
    kept = sort!([(uses, key, closed) for (key, (uses, closed)) in seen if uses >= min_count]; by = t -> (-t[1], codeunits(t[2])))
    rest = [(uses, closed) for (_, (uses, closed)) in seen if uses < min_count]
    other_uses = sum((uses for (uses, _) in rest); init = 0)
    other_closed = sum((closed for (_, closed) in rest); init = 0)
    TemplateVocabulary(vcat(OTHER, String[key for (_, key, _) in kept]), vcat(other_uses, Int[uses for (uses, _, _) in kept]),
                       vcat(other_uses == 0 ? 0.0 : other_closed / other_uses, Float64[closed / uses for (uses, _, closed) in kept]))
end
