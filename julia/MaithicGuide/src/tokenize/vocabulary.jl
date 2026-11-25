# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The tokens a model knows.

"The token that fills a sequence up to a length."
const PAD = "<pad>"
"An operator or a bracket the vocabulary does not hold."
const UNKNOWN = "<unk>"
"A name the vocabulary does not hold."
const UNKNOWN_NAME = "<id>"
"A number the vocabulary does not hold."
const UNKNOWN_NUMBER = "<num>"
"A string or a character the vocabulary does not hold."
const UNKNOWN_STRING = "<str>"

const SPECIALS = [PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING]

"""
    Vocabulary

Tokens with the number of times each was seen. The first five are special:
padding, and one stand-in for each sort of token the vocabulary does not
hold, so that an unknown name and an unknown number stay different things to
the model.
"""
struct Vocabulary
    tokens::Vector{String}
    counts::Vector{Int}
    index::Dict{String,Int}
end

function Vocabulary(tokens::Vector{String}, counts::Vector{Int})
    length(tokens) == length(counts) || throw(ArgumentError("a vocabulary needs one count for each token"))
    length(tokens) >= length(SPECIALS) && tokens[1:length(SPECIALS)] == SPECIALS ||
        throw(ArgumentError("a vocabulary begins with the special tokens $(join(SPECIALS, ", "))"))
    index = Dict{String,Int}()
    for (i, token) in enumerate(tokens)
        haskey(index, token) && throw(ArgumentError("the token $(repr(token)) is in the vocabulary twice"))
        index[token] = i
    end
    Vocabulary(tokens, counts, index)
end

Base.length(v::Vocabulary) = length(v.tokens)
Base.:(==)(a::Vocabulary, b::Vocabulary) = a.tokens == b.tokens && a.counts == b.counts

"""
    build_vocabulary(texts; min_count = 2, max_size = typemax(Int))

The vocabulary of a collection of Lean texts: every token seen at least
`min_count` times, the most frequent first and equally frequent ones in the
order of their text, at most `max_size` tokens with the special ones. The
result depends on the texts as a collection, not on their order.
"""
function build_vocabulary(texts; min_count::Integer = 2, max_size::Integer = typemax(Int))
    min_count >= 1 || throw(ArgumentError("min_count must be at least 1"))
    max_size >= length(SPECIALS) || throw(ArgumentError("max_size must leave room for the $(length(SPECIALS)) special tokens"))
    seen = Dict{String,Int}()
    for text in texts, token in lex(text)
        seen[token.text] = get(seen, token.text, 0) + 1
    end
    for special in SPECIALS
        delete!(seen, special)
    end
    kept = sort!([(count, token) for (token, count) in seen if count >= min_count]; by = pair -> (-pair[1], codeunits(pair[2])))
    kept = kept[1:min(length(kept), max_size - length(SPECIALS))]
    Vocabulary(vcat(SPECIALS, String[token for (_, token) in kept]), vcat(zeros(Int, length(SPECIALS)), Int[count for (count, _) in kept]))
end

"""
    token_id(vocabulary, token)

The number of a token: its own when the vocabulary holds it, and otherwise
that of the stand-in for its sort.
"""
function token_id(v::Vocabulary, token::Token)
    known = get(v.index, token.text, 0)
    known != 0 && return known
    token.kind === :identifier ? 3 : token.kind === :number ? 4 : token.kind === :string ? 5 : 2
end
