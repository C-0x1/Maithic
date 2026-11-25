# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# What a model says about the candidates of a search.

"""
    tactic_priors(model, state, candidates)

A probability for each candidate tactic on a proof state; they add up to 1.
A candidate has the probability of its template, shared equally among the
candidates with that template, and candidates whose template the model does
not know share the probability of all other tactics. The result is then
scaled to the candidates that were given: it answers "which of these", not
"which of all tactics".
"""
function tactic_priors(model::GuideModel, state::AbstractString, candidates::AbstractVector{<:AbstractString})
    isempty(candidates) && return Float64[]
    p = template_probabilities(model, state)
    classes = [class_of(model.templates, c) for c in candidates]
    sharing = Dict{Int,Int}()
    for class in classes
        sharing[class] = get(sharing, class, 0) + 1
    end
    weights = [p[class] / sharing[class] for class in classes]
    total = sum(weights)
    total > 0 ? weights ./ total : fill(1.0 / length(candidates), length(candidates))
end

"""
    state_value(model, state)

An estimate, between 0 and 1, of how near a proof state is to being closed:
the chance that the next tactic closes a goal, which is the probability of
each template times how often tactics of that template closed a goal in the
training data.
"""
function state_value(model::GuideModel, state::AbstractString)
    p = template_probabilities(model, state)
    value = 0.0
    for (i, rate) in enumerate(model.templates.closes)
        value += p[i] * rate
    end
    clamp(value, 0.0, 1.0)
end

# The parts of a name: `List.length_append` is made of List, length and append.
function name_parts(name::AbstractString)
    parts = String[]
    for piece in split(name, ['.', '_']; keepempty = false)
        word = Char[]
        for c in piece
            if isuppercase(c) && !isempty(word) && islowercase(word[end])
                push!(parts, lowercase(String(word)))
                empty!(word)
            end
            push!(word, c)
        end
        isempty(word) || push!(parts, lowercase(String(word)))
    end
    parts
end

"""
    premise_scores(model, state, premises)

A score for each premise name, higher for the ones more likely to be of use
on the state: the share of the parts of the name that occur in the state,
each part weighted so that rare ones count for more. The weights come from
the token counts of the model's vocabulary. This is a lexical ranking, a
first sieve; it knows nothing of what a premise states.
"""
function premise_scores(model::GuideModel, state::AbstractString, premises::AbstractVector{<:AbstractString})
    present = Set{String}()
    for token in lex(main_goal(state))
        token.kind === :identifier && union!(present, name_parts(token.text))
    end
    counts = Dict{String,Int}()
    for (token, count) in zip(model.vocabulary.tokens, model.vocabulary.counts), part in name_parts(token)
        counts[part] = get(counts, part, 0) + count
    end
    weight(part) = 1.0 / log(2.0 + get(counts, part, 0))
    map(premises) do premise
        parts = name_parts(premise)
        isempty(parts) && return 0.0
        sum((weight(part) for part in parts if part in present); init = 0.0) / sum(weight(part) for part in parts)
    end
end
