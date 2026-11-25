# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading expressions and relations from text.
#
#   conjunction := chain (AND chain)*            AND is one of  ∧  /\  &&  and
#   chain       := sum (REL sum)+                0 <= x < 1 means 0 <= x and x < 1
#   sum         := product (('+' | '-') product)*
#   product     := unary (('*' | '/') unary)*
#   unary       := '-' unary | power
#   power       := atom ('^' natural)?
#   atom        := number | name | name '(' sum (',' sum)* ')' | '(' sum ')'
#
# As in Lean, -x^2 is -(x^2) and -x * y is (-x) * y.

"""
    ParseError(message, position)

The text is not an expression of the exchange format. `position` counts
characters from one.
"""
struct ParseError <: Exception
    message::String
    position::Int
end

Base.showerror(io::IO, e::ParseError) =
    print(io, "cannot read the expression at character ", e.position, ": ", e.message)

struct Token
    kind::Symbol      # :number, :name, :symbol, :relation, :and or :end
    text::String
    position::Int
end

const RELATIONS = Dict("<=" => :le, "≤" => :le, "<" => :lt, ">=" => :ge, "≥" => :ge, ">" => :gt,
                       "=" => :eq, "==" => :eq, "!=" => :ne, "≠" => :ne)

is_ascii_digit(c::Char) = '0' <= c <= '9'
is_name_start(c::Char) = isletter(c) || c == '_'
is_name_part(c::Char) = isletter(c) || is_ascii_digit(c) || c == '_' || c == '\'' || '₀' <= c <= '₉'

function tokenize(text::AbstractString)
    chars = collect(text)
    n = length(chars)
    tokens = Token[]
    i = 1
    while i <= n
        c = chars[i]
        pair = i < n ? string(c, chars[i + 1]) : ""
        if isspace(c)
            i += 1
        elseif is_ascii_digit(c)
            j = i
            while j <= n && is_ascii_digit(chars[j])
                j += 1
            end
            if j < n && chars[j] == '.' && is_ascii_digit(chars[j + 1])
                j += 1
                while j <= n && is_ascii_digit(chars[j])
                    j += 1
                end
            end
            if j <= n && chars[j] in ('e', 'E')
                k = j + 1
                k <= n && chars[k] in ('+', '-') && (k += 1)
                if k <= n && is_ascii_digit(chars[k])
                    while k <= n && is_ascii_digit(chars[k])
                        k += 1
                    end
                    j = k
                end
            end
            push!(tokens, Token(:number, String(chars[i:(j - 1)]), i))
            i = j
        elseif is_name_start(c)
            j = i + 1
            while j <= n && is_name_part(chars[j])
                j += 1
            end
            word = String(chars[i:(j - 1)])
            push!(tokens, Token(word == "and" ? :and : :name, word, i))
            i = j
        elseif pair == "/\\" || pair == "&&"
            push!(tokens, Token(:and, pair, i))
            i += 2
        elseif haskey(RELATIONS, pair)
            push!(tokens, Token(:relation, pair, i))
            i += 2
        elseif haskey(RELATIONS, string(c))
            push!(tokens, Token(:relation, string(c), i))
            i += 1
        elseif c == '∧'
            push!(tokens, Token(:and, "∧", i))
            i += 1
        elseif c in ('+', '-', '*', '/', '^', '(', ')', ',')
            push!(tokens, Token(:symbol, string(c), i))
            i += 1
        elseif c == '−'
            push!(tokens, Token(:symbol, "-", i))
            i += 1
        else
            throw(ParseError("the character '$(c)' has no meaning here", i))
        end
    end
    push!(tokens, Token(:end, "", n + 1))
end

mutable struct Parser
    tokens::Vector{Token}
    next::Int
end

current(p::Parser) = p.tokens[p.next]
advance!(p::Parser) = (token = p.tokens[p.next]; p.next += 1; token)
is_symbol(token::Token, text::String) = token.kind == :symbol && token.text == text

function expect_symbol!(p::Parser, text::String)
    token = advance!(p)
    is_symbol(token, text) || throw(ParseError("\"$(text)\" is expected", token.position))
end

function expect_end!(p::Parser)
    token = current(p)
    token.kind == :end || throw(ParseError("\"$(token.text)\" is not expected here", token.position))
end

function parse_sum!(p::Parser)
    left = parse_product!(p)
    while true
        token = current(p)
        if is_symbol(token, "+")
            advance!(p)
            left = BinOp(:+, left, parse_product!(p))
        elseif is_symbol(token, "-")
            advance!(p)
            left = BinOp(:-, left, parse_product!(p))
        else
            return left
        end
    end
end

function parse_product!(p::Parser)
    left = parse_unary!(p)
    while true
        token = current(p)
        if is_symbol(token, "*")
            advance!(p)
            left = BinOp(:*, left, parse_unary!(p))
        elseif is_symbol(token, "/")
            advance!(p)
            left = BinOp(:/, left, parse_unary!(p))
        else
            return left
        end
    end
end

function parse_unary!(p::Parser)
    if is_symbol(current(p), "-")
        advance!(p)
        literal = current(p).kind == :number
        arg = parse_unary!(p)
        # A minus sign directly before a number makes a negative number; -2^2
        # is still -(2^2) because its operand is a power, not a number, and
        # -0 stays a negation because there is no negative zero to make.
        return literal && arg isa Num && !iszero(arg.value) ? Num(-arg.value) : Neg(arg)
    end
    parse_power!(p)
end

function parse_power!(p::Parser)
    base = parse_atom!(p)
    is_symbol(current(p), "^") || return base
    advance!(p)
    token = advance!(p)
    exponent = token.kind == :number ? tryparse(Int, token.text) : nothing
    (exponent === nothing || exponent > 10_000) &&
        throw(ParseError("an exponent must be a natural number", token.position))
    is_symbol(current(p), "^") &&
        throw(ParseError("a tower of exponents needs parentheses", current(p).position))
    Pow(base, exponent)
end

function parse_atom!(p::Parser)
    token = advance!(p)
    if token.kind == :number
        Num(parse_exact(token.text))
    elseif token.kind == :name && is_symbol(current(p), "(")
        fn = Symbol(token.text)
        haskey(FUNCTIONS, fn) || throw(ParseError("\"$(token.text)\" is not a known function", token.position))
        advance!(p)
        args = Node[parse_sum!(p)]
        while is_symbol(current(p), ",")
            advance!(p)
            push!(args, parse_sum!(p))
        end
        expect_symbol!(p, ")")
        length(args) == FUNCTIONS[fn] || throw(ParseError(
            "$(token.text) takes $(FUNCTIONS[fn]) argument$(FUNCTIONS[fn] == 1 ? "" : "s"), not $(length(args))",
            token.position))
        Call(fn, args)
    elseif token.kind == :name
        Var(token.text)
    elseif is_symbol(token, "(")
        inner = parse_sum!(p)
        expect_symbol!(p, ")")
        inner
    elseif token.kind == :end
        throw(ParseError("the expression ends too early", token.position))
    else
        throw(ParseError("\"$(token.text)\" is not expected here", token.position))
    end
end

function parse_chain!(p::Parser)
    relations = Relation[]
    left = parse_sum!(p)
    while true
        token = current(p)
        if token.kind != :relation
            isempty(relations) && throw(ParseError("a comparison such as <= is expected", token.position))
            return relations
        end
        advance!(p)
        right = parse_sum!(p)
        push!(relations, Relation(RELATIONS[token.text], left, right))
        left = right
    end
end

"""
    parse_expression(text)

Reads an arithmetic expression: exact numbers (`3`, `0.25`, `1e-3`),
variables, `+ - * /`, powers with a natural-number exponent, parentheses and
the functions `sqrt exp log sin cos abs min max`. A [`ParseError`](@ref) says
where reading stopped and why.
"""
function parse_expression(text::AbstractString)
    p = Parser(tokenize(text), 1)
    node = parse_sum!(p)
    expect_end!(p)
    node
end

"""
    parse_relation(text)

Reads one comparison of two expressions. The comparisons are `<=`, `<`, `>=`,
`>`, `=` and `!=`, also written `≤`, `≥`, `≠` and `==`.
"""
function parse_relation(text::AbstractString)
    relations = parse_conjunction(text)
    length(relations) == 1 || throw(ParseError("one comparison is expected, not $(length(relations))", 1))
    relations[1]
end

"""
    parse_conjunction(text)

Reads comparisons joined by `∧` (also `/\\`, `&&` or `and`). A chain such as
`0 <= x < 1` stands for its links, `0 <= x` and `x < 1`.
"""
function parse_conjunction(text::AbstractString)
    p = Parser(tokenize(text), 1)
    relations = parse_chain!(p)
    while current(p).kind == :and
        advance!(p)
        append!(relations, parse_chain!(p))
    end
    expect_end!(p)
    relations
end
