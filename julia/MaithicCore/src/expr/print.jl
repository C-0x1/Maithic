# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Writing expressions as text: plain, which parses back, and Lean.

# Binding strength: 1 sum, 2 product, 3 negation, 4 power, 5 atom.
strength(n::Num) = denominator(n.value) != 1 ? 2 : n.value < 0 ? 3 : 5
strength(::Var) = 5
strength(::Call) = 5
strength(::Pow) = 4
strength(::Neg) = 3
strength(n::BinOp) = n.op in (:+, :-) ? 1 : 2

const PLAIN_RELATIONS = Dict(:le => "<=", :lt => "<", :ge => ">=", :gt => ">", :eq => "=", :ne => "!=")
const LEAN_RELATIONS = Dict(:le => "≤", :lt => "<", :ge => "≥", :gt => ">", :eq => "=", :ne => "≠")
const LEAN_FUNCTIONS = Dict(:sqrt => "Real.sqrt", :exp => "Real.exp", :log => "Real.log", :sin => "Real.sin",
                            :cos => "Real.cos", :min => "min", :max => "max")

"""
    expression_text(node; style = :plain)

The text of an expression with as few parentheses as its structure allows.
With `style = :plain` the text is read back by [`parse_expression`](@ref) to
an expression with the same value; with `style = :lean` it is a Lean term,
with `Real.sqrt x` for `sqrt(x)` and `|x|` for `abs(x)`.
"""
function expression_text(node::Node; style::Symbol = :plain)
    style in (:plain, :lean) || throw(ArgumentError("style must be :plain or :lean"))
    io = IOBuffer()
    show_node(io, node, 1, style)
    String(take!(io))
end

"""
    relation_text(relation; style = :plain)

The text of a relation, in the same two styles as [`expression_text`](@ref).
"""
function relation_text(r::Relation; style::Symbol = :plain)
    symbols = style == :lean ? LEAN_RELATIONS : PLAIN_RELATIONS
    string(expression_text(r.left; style), " ", symbols[r.op], " ", expression_text(r.right; style))
end

Base.show(io::IO, n::Node) = print(io, expression_text(n))
Base.show(io::IO, r::Relation) = print(io, relation_text(r))

function show_node(io::IO, node::Node, minimum::Int, style::Symbol)
    wrap = strength(node) < minimum
    wrap && print(io, "(")
    show_bare(io, node, style)
    wrap && print(io, ")")
end

function show_bare(io::IO, n::Num, ::Symbol)
    if denominator(n.value) == 1
        print(io, numerator(n.value))
    else
        print(io, numerator(n.value), " / ", denominator(n.value))
    end
end

show_bare(io::IO, n::Var, ::Symbol) = print(io, n.name)

function show_bare(io::IO, n::Neg, style::Symbol)
    print(io, "-")
    show_node(io, n.arg, 4, style)
end

function show_bare(io::IO, n::BinOp, style::Symbol)
    level = strength(n)
    show_node(io, n.left, level, style)
    print(io, " ", n.op, " ")
    show_node(io, n.right, level + 1, style)
end

function show_bare(io::IO, n::Pow, style::Symbol)
    show_node(io, n.base, 5, style)
    print(io, style == :lean ? " ^ " : "^", n.exponent)
end

function show_bare(io::IO, n::Call, style::Symbol)
    if style == :plain
        print(io, n.fn, "(")
        for (i, arg) in enumerate(n.args)
            i > 1 && print(io, ", ")
            show_node(io, arg, 1, style)
        end
        print(io, ")")
    elseif n.fn == :abs
        print(io, "|")
        show_node(io, n.args[1], 1, style)
        print(io, "|")
    else
        print(io, "(", LEAN_FUNCTIONS[n.fn])
        for arg in n.args
            print(io, " ")
            show_node(io, arg, 5, style)
        end
        print(io, ")")
    end
end
