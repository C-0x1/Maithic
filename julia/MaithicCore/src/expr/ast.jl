# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The arithmetic language of the oracle exchange format, as a tree.

"A node of an arithmetic expression."
abstract type Node end

"A number, held exactly."
struct Num <: Node
    value::Exact
end

"A variable."
struct Var <: Node
    name::String
end

"The negation of an expression."
struct Neg <: Node
    arg::Node
end

"A sum, difference, product or quotient; `op` is one of `:+`, `:-`, `:*`, `:/`."
struct BinOp <: Node
    op::Symbol
    left::Node
    right::Node
end

"An expression raised to a natural-number power."
struct Pow <: Node
    base::Node
    exponent::Int
    function Pow(base::Node, exponent::Integer)
        exponent >= 0 || throw(ArgumentError("an exponent must be a natural number"))
        new(base, Int(exponent))
    end
end

"An application of one of the [`FUNCTIONS`](@ref)."
struct Call <: Node
    fn::Symbol
    args::Vector{Node}
end

"""
    Relation(op, left, right)

A comparison of two expressions; `op` is one of `:le`, `:lt`, `:ge`, `:gt`,
`:eq`, `:ne`.
"""
struct Relation
    op::Symbol
    left::Node
    right::Node
end

"The functions of the language, with the number of arguments each takes."
const FUNCTIONS = Dict{Symbol,Int}(:sqrt => 1, :exp => 1, :log => 1, :sin => 1, :cos => 1, :abs => 1,
                                   :min => 2, :max => 2)

same_fields(a, b) = all(getfield(a, i) == getfield(b, i) for i in 1:nfields(a))
hash_fields(a, h::UInt) = foldl((acc, i) -> hash(getfield(a, i), acc), 1:nfields(a); init = hash(typeof(a), h))

Base.:(==)(a::Node, b::Node) = typeof(a) == typeof(b) && same_fields(a, b)
Base.hash(a::Node, h::UInt) = hash_fields(a, h)
Base.:(==)(a::Relation, b::Relation) = same_fields(a, b)
Base.hash(a::Relation, h::UInt) = hash_fields(a, h)

# Building trees in code reads like the mathematics.
as_node(x::Node) = x
as_node(x::Union{Integer,Rational}) = Num(exact(x))

for (op, name) in ((:+, :(:+)), (:-, :(:-)), (:*, :(:*)), (:/, :(:/)))
    @eval begin
        Base.$op(a::Node, b::Node) = BinOp($name, a, b)
        Base.$op(a::Node, b::Union{Integer,Rational}) = BinOp($name, a, as_node(b))
        Base.$op(a::Union{Integer,Rational}, b::Node) = BinOp($name, as_node(a), b)
    end
end
Base.:-(a::Node) = Neg(a)
Base.:^(a::Node, n::Integer) = Pow(a, n)

"""
    variables(x)

The names of the variables of an expression, a relation or a list of them,
sorted and without repeats.
"""
variables(x) = sort!(collect(collect_variables!(Set{String}(), x)))

collect_variables!(names::Set{String}, ::Num) = names
collect_variables!(names::Set{String}, n::Var) = push!(names, n.name)
collect_variables!(names::Set{String}, n::Neg) = collect_variables!(names, n.arg)
collect_variables!(names::Set{String}, n::Pow) = collect_variables!(names, n.base)
collect_variables!(names::Set{String}, n::BinOp) =
    collect_variables!(collect_variables!(names, n.left), n.right)
collect_variables!(names::Set{String}, r::Relation) =
    collect_variables!(collect_variables!(names, r.left), r.right)
collect_variables!(names::Set{String}, n::Call) = foldl(collect_variables!, n.args; init = names)
collect_variables!(names::Set{String}, xs::Union{AbstractVector,Tuple}) = foldl(collect_variables!, xs; init = names)

"""
    substitute(x, bindings)

`x` with each variable named in `bindings` replaced by the expression bound
to it. Variables that are not named stay.
"""
substitute(n::Num, _) = n
substitute(n::Var, bindings) = haskey(bindings, n.name) ? as_node(bindings[n.name]) : n
substitute(n::Neg, bindings) = Neg(substitute(n.arg, bindings))
substitute(n::BinOp, bindings) = BinOp(n.op, substitute(n.left, bindings), substitute(n.right, bindings))
substitute(n::Pow, bindings) = Pow(substitute(n.base, bindings), n.exponent)
substitute(n::Call, bindings) = Call(n.fn, Node[substitute(a, bindings) for a in n.args])
substitute(r::Relation, bindings) = Relation(r.op, substitute(r.left, bindings), substitute(r.right, bindings))

const NEGATED = Dict(:le => :gt, :gt => :le, :lt => :ge, :ge => :lt, :eq => :ne, :ne => :eq)

"The relation that holds exactly when `r` does not."
negate(r::Relation) = Relation(NEGATED[r.op], r.left, r.right)
