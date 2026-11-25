# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Lean text as a sequence of tokens.
#
# A model reads a proof state as tokens, and what counts as one token decides
# what it can learn. `Nat.succ_le_of_lt` is one name, `≤` is one operator and
# `h✝` is one hypothesis; splitting them by bytes or by spaces would lose that.

"""
    Token(kind, text)

A piece of Lean text. `kind` is `:identifier`, `:operator`, `:number`,
`:string` or `:punctuation`; numbers and strings are the literals.
"""
struct Token
    kind::Symbol
    text::String
end

Base.:(==)(a::Token, b::Token) = a.kind == b.kind && a.text == b.text
Base.hash(t::Token, h::UInt) = hash(t.text, hash(t.kind, h))

"Whether a token is a literal: a number, a string or a character."
isliteral(t::Token) = t.kind === :number || t.kind === :string

const BRACKETS = Set(['(', ')', '[', ']', '{', '}', '⟨', '⟩', ',', ';'])

# Operators of more than one character, longest first. Anything else that is
# neither a name, a literal nor a bracket is an operator of one character,
# which covers the symbols of mathematics.
const LONG_OPERATORS = sort!(["<;>", "<|>", "|>.", "<->", ">>=", "<\$>", "<*>", ":=", "=>", "->", "<-", "::", "++", "<=", ">=", "!=", "&&", "||",
                              "|>", "<|", "..", "^^", "=="]; by = length, rev = true)

is_subscript(c::Char) = '₀' <= c <= 'ₜ'
is_superscript(c::Char) = c == '¹' || c == '²' || c == '³' || '⁰' <= c <= '⁹'
name_start(c::Char) = isletter(c) || c == '_'
name_part(c::Char) = isletter(c) || isdigit(c) || c == '_' || c == '\'' || c == '!' || c == '?' || c == '✝' || is_subscript(c) || is_superscript(c)

"""
    lex(text)

The tokens of a Lean text, in order. Space and comments separate tokens and
are not tokens. A dotted name is one identifier, a name in guillemets is one
identifier, and a text that ends inside a string or a comment ends there:
`lex` never fails, because a proof state may be cut off.
"""
function lex(text::AbstractString)
    chars = collect(text)
    n = length(chars)
    tokens = Token[]
    i = 1
    at(k) = 1 <= k <= n ? chars[k] : '\0'
    slice(a, b) = String(chars[a:b])
    while i <= n
        c = chars[i]
        if isspace(c)
            i += 1
        elseif c == '-' && at(i + 1) == '-'
            while i <= n && chars[i] != '\n'
                i += 1
            end
        elseif c == '/' && at(i + 1) == '-'
            i += 2
            while i <= n && !(chars[i] == '-' && at(i + 1) == '/')
                i += 1
            end
            i += 2
        elseif c == '«'
            stop = i + 1
            while stop <= n && chars[stop] != '»'
                stop += 1
            end
            stop = min(stop, n)
            push!(tokens, Token(:identifier, slice(i, stop)))
            i = stop + 1
        elseif name_start(c)
            stop = i
            while true
                while stop < n && name_part(chars[stop + 1])
                    stop += 1
                end
                # A dot continues the name when a name follows it.
                if at(stop + 1) == '.' && name_start(at(stop + 2))
                    stop += 2
                else
                    break
                end
            end
            push!(tokens, Token(:identifier, slice(i, stop)))
            i = stop + 1
        elseif isdigit(c)
            stop = i
            if c == '0' && (at(i + 1) == 'x' || at(i + 1) == 'X') && isxdigit(at(i + 2))
                stop = i + 2
                while stop < n && isxdigit(chars[stop + 1])
                    stop += 1
                end
            else
                while stop < n && isdigit(chars[stop + 1])
                    stop += 1
                end
                # `3.14` is one number; in `p.1.2` the digits are two projections.
                if at(stop + 1) == '.' && isdigit(at(stop + 2)) && at(i - 1) != '.'
                    stop += 2
                    while stop < n && isdigit(chars[stop + 1])
                        stop += 1
                    end
                end
            end
            push!(tokens, Token(:number, slice(i, stop)))
            i = stop + 1
        elseif c == '"'
            stop = i + 1
            while stop <= n && chars[stop] != '"'
                stop += chars[stop] == '\\' ? 2 : 1
            end
            stop = min(stop, n)
            push!(tokens, Token(:string, slice(i, stop)))
            i = stop + 1
        elseif c == '\'' && ((at(i + 1) == '\\' && at(i + 3) == '\'') || (at(i + 1) != '\\' && at(i + 2) == '\''))
            stop = at(i + 1) == '\\' ? i + 3 : i + 2
            push!(tokens, Token(:string, slice(i, stop)))
            i = stop + 1
        elseif c in BRACKETS
            push!(tokens, Token(:punctuation, string(c)))
            i += 1
        else
            found = false
            for operator in LONG_OPERATORS
                width = length(operator)
                if i + width - 1 <= n && slice(i, i + width - 1) == operator
                    push!(tokens, Token(:operator, operator))
                    i += width
                    found = true
                    break
                end
            end
            if !found
                push!(tokens, Token(c == '.' ? :punctuation : :operator, string(c)))
                i += 1
            end
        end
    end
    tokens
end
