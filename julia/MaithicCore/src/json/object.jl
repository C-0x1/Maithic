# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# JSON objects that keep the order of their members.

"""
    Object(pairs...)

A JSON object that remembers the order in which its members were added. A
document read with [`read_json`](@ref) and written back with
[`write_json`](@ref) keeps its layout, and a document built in code is written
in the order the code states, which is what makes output reproducible.

Two objects are equal when they have the same members, in any order.
"""
struct Object
    names::Vector{String}
    values::Vector{Any}
    index::Dict{String,Int}
    Object() = new(String[], Any[], Dict{String,Int}())
end

function Object(pairs::Pair...)
    object = Object()
    for (name, value) in pairs
        object[String(name)] = value
    end
    object
end

Base.length(o::Object) = length(o.names)
Base.isempty(o::Object) = isempty(o.names)
Base.haskey(o::Object, name::AbstractString) = haskey(o.index, name)
Base.keys(o::Object) = copy(o.names)
Base.getindex(o::Object, name::AbstractString) =
    haskey(o.index, name) ? o.values[o.index[name]] : throw(KeyError(String(name)))
Base.get(o::Object, name::AbstractString, default) = haskey(o.index, name) ? o.values[o.index[name]] : default

function Base.setindex!(o::Object, value, name::AbstractString)
    position = get(o.index, name, 0)
    if position == 0
        push!(o.names, String(name))
        push!(o.values, value)
        o.index[String(name)] = length(o.names)
    else
        o.values[position] = value
    end
    o
end

Base.iterate(o::Object, i::Int = 1) = i > length(o.names) ? nothing : (o.names[i] => o.values[i], i + 1)
Base.eltype(::Type{Object}) = Pair{String,Any}

Base.:(==)(a::Object, b::Object) =
    length(a) == length(b) && all(haskey(b, name) && value == b[name] for (name, value) in a)
Base.hash(o::Object, h::UInt) = hash(sort!(collect(o.names)), hash(:MaithicObject, h))

Base.show(io::IO, o::Object) = print(io, write_json(o))
