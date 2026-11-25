# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# A proof state as a model reads it.

"""
    main_goal(state)

The first goal of a proof state: the one a tactic acts on. Goals are
separated by an empty line.
"""
function main_goal(state::AbstractString)
    stop = findfirst("\n\n", state)
    String(stop === nothing ? state : state[1:prevind(state, first(stop))])
end

"""
    conclusion_first(goal)

A goal with its conclusion, the part from `⊢` on, put before its hypotheses.
A model reads a limited number of tokens, and of a goal with many hypotheses
the conclusion says most about the next tactic. Read first, it is never the
part that is cut off, and it always starts at the same position.
"""
function conclusion_first(goal::AbstractString)
    lines = split(goal, '\n')
    at = findlast(line -> startswith(line, "⊢"), lines)
    at === nothing || at == 1 ? String(goal) : join(vcat(lines[at:end], lines[1:at - 1]), "\n")
end

"""
    state_ids(vocabulary, state; max_length = typemax(Int))

The token numbers a model reads for a proof state: those of its first goal,
conclusion first, cut to `max_length`. Training, evaluation and the answers
to a search all read a state this way.
"""
state_ids(v::Vocabulary, state::AbstractString; max_length::Integer = typemax(Int)) =
    encode(v, conclusion_first(main_goal(state)); max_length = max_length)
