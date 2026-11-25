# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Writing JSON. The same value always gives the same text.

"""
    write_json(value; pretty = false, canonical = false)

The JSON text of a value.

| Julia value | JSON |
|---|---|
| `nothing` | `null` |
| `Bool` | `true`, `false` |
| `Integer`, of any size | a number, every digit kept |
| finite `AbstractFloat` | the shortest number that reads back to the same `Float64` |
| `AbstractString`, `Symbol`, `Char` | a string |
| `AbstractVector`, `Tuple` | an array |
| [`Object`](@ref), `NamedTuple` | an object, members in their own order |
| `AbstractDict` | an object, members sorted by name |

A `Rational` is refused: exact numbers are written as text with
[`exact_text`](@ref), so that the choice is visible where it is made. With
`canonical = true` the members of every object are sorted by name, which makes
the text a function of the value alone. With `pretty = true` the text is
indented by two spaces and arrays of plain values stay on one line.
"""
function write_json(value; pretty::Bool = false, canonical::Bool = false)
    io = IOBuffer()
    emit(io, value, pretty ? 0 : -1, canonical)
    String(take!(io))
end

emit(io::IO, ::Nothing, ::Int, ::Bool) = print(io, "null")
emit(io::IO, value::Bool, ::Int, ::Bool) = print(io, value ? "true" : "false")
emit(io::IO, value::Integer, ::Int, ::Bool) = print(io, value)
emit(io::IO, value::AbstractString, ::Int, ::Bool) = emit_string(io, value)
emit(io::IO, value::Symbol, ::Int, ::Bool) = emit_string(io, String(value))
emit(io::IO, value::Char, ::Int, ::Bool) = emit_string(io, string(value))
emit(::IO, value::Rational, ::Int, ::Bool) =
    throw(ArgumentError("the rational $(value) must be written as text; use exact_text"))

function emit(io::IO, value::AbstractFloat, ::Int, ::Bool)
    isfinite(value) || throw(ArgumentError("JSON has no notation for $(value)"))
    print(io, repr(Float64(value)))
end

emit(io::IO, value::Union{AbstractVector,Tuple}, level::Int, canonical::Bool) =
    emit_array(io, value, level, canonical)
emit(io::IO, value::Object, level::Int, canonical::Bool) =
    emit_members(io, canonical ? sort!(collect(value); by = first) : collect(value), level, canonical)
emit(io::IO, value::AbstractDict, level::Int, canonical::Bool) =
    emit_members(io, sort!([String(k) => v for (k, v) in value]; by = first), level, canonical)

function emit(io::IO, value::NamedTuple, level::Int, canonical::Bool)
    members = [String(k) => v for (k, v) in pairs(value)]
    emit_members(io, canonical ? sort!(members; by = first) : members, level, canonical)
end

emit(::IO, value, ::Int, ::Bool) = throw(ArgumentError("no JSON form for a value of type $(typeof(value))"))

is_plain(value) = value isa Union{Nothing,Bool,Number,AbstractString,Symbol,Char}

function emit_array(io::IO, items, level::Int, canonical::Bool)
    if isempty(items)
        print(io, "[]")
    elseif level < 0 || all(is_plain, items)
        print(io, "[")
        for (i, item) in enumerate(items)
            i > 1 && print(io, level < 0 ? "," : ", ")
            emit(io, item, level, canonical)
        end
        print(io, "]")
    else
        print(io, "[")
        for (i, item) in enumerate(items)
            print(io, i > 1 ? ",\n" : "\n", "  "^(level + 1))
            emit(io, item, level + 1, canonical)
        end
        print(io, "\n", "  "^level, "]")
    end
end

function emit_members(io::IO, members, level::Int, canonical::Bool)
    if isempty(members)
        print(io, "{}")
        return
    end
    print(io, "{")
    for (i, (name, value)) in enumerate(members)
        i > 1 && print(io, ",")
        level >= 0 && print(io, "\n", "  "^(level + 1))
        emit_string(io, name)
        print(io, level < 0 ? ":" : ": ")
        emit(io, value, level < 0 ? level : level + 1, canonical)
    end
    level >= 0 && print(io, "\n", "  "^level)
    print(io, "}")
end

function emit_string(io::IO, text::AbstractString)
    isvalid(text) || throw(ArgumentError("a JSON string must be valid UTF-8"))
    print(io, '"')
    for c in text
        if c == '"'
            print(io, "\\\"")
        elseif c == '\\'
            print(io, "\\\\")
        elseif c == '\n'
            print(io, "\\n")
        elseif c == '\t'
            print(io, "\\t")
        elseif c == '\r'
            print(io, "\\r")
        elseif c < ' '
            print(io, "\\u", string(UInt32(c); base = 16, pad = 4))
        else
            print(io, c)
        end
    end
    print(io, '"')
end
