# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Text to numbers and back.

"""
    encode(vocabulary, text; max_length = typemax(Int))

The numbers of the tokens of a text, the first `max_length` of them.
"""
function encode(v::Vocabulary, text::AbstractString; max_length::Integer = typemax(Int))
    tokens = lex(text)
    Int[token_id(v, tokens[i]) for i in 1:min(length(tokens), max_length)]
end

"""
    decode(vocabulary, ids)

The tokens with the given numbers, joined by spaces. Padding is left out.
For a text whose tokens the vocabulary holds, `decode(v, encode(v, text))`
is the text with one space between its tokens; a token the vocabulary does
not hold comes back as the stand-in for its sort.
"""
function decode(v::Vocabulary, ids)
    for id in ids
        1 <= id <= length(v) || throw(ArgumentError("$(id) is not the number of a token; the vocabulary has $(length(v))"))
    end
    join((v.tokens[id] for id in ids if id != 1), " ")
end
