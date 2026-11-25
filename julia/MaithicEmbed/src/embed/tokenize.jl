# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Tokenization of Lean names and statements.
#
# A declaration name such as `Nat.add_comm` carries most of its meaning in its
# parts, so names are split at dots, underscores and case changes. Symbols such
# as `∀`, `→` and `≤` are tokens of their own.

const SYMBOL_NAMES = Dict(
    '∀' => "forall", '∃' => "exists", '→' => "implies", '↔' => "iff", '∧' => "and", '∨' => "or", '¬' => "not",
    '≤' => "le", '≥' => "ge", '≠' => "ne", '∈' => "mem", '∉' => "notmem", '⊆' => "subset", '∘' => "comp",
    '×' => "prod", 'λ' => "fun", '∑' => "sum", '∏' => "product", '⊢' => "goal", '+' => "plus", '-' => "minus",
    '*' => "times", '/' => "div", '%' => "mod", '^' => "pow", '=' => "eq", '<' => "lt", '>' => "gt",
)

"Splits one identifier part at case changes: `addComm` gives `add`, `comm`."
function split_case(part::AbstractString)
    pieces = String[]
    current = IOBuffer()
    previous_lower = false
    for c in part
        if isuppercase(c) && previous_lower
            push!(pieces, String(take!(current)))
        end
        print(current, lowercase(c))
        previous_lower = islowercase(c) || isdigit(c)
    end
    last_piece = String(take!(current))
    isempty(last_piece) || push!(pieces, last_piece)
    pieces
end

"""
    tokenize(text)

The tokens of a statement or a name, lowercased: identifier parts split at
dots, underscores and case changes, numbers, and one token per mathematical
symbol.
"""
function tokenize(text::AbstractString)
    tokens = String[]
    word = IOBuffer()
    flush_word() = begin
        w = String(take!(word))
        isempty(w) || append!(tokens, split_case(w))
    end
    for c in text
        if isletter(c) && !haskey(SYMBOL_NAMES, c) || isdigit(c) || c == '\''
            print(word, c)
        else
            flush_word()
            haskey(SYMBOL_NAMES, c) && push!(tokens, SYMBOL_NAMES[c])
        end
    end
    flush_word()
    tokens
end
