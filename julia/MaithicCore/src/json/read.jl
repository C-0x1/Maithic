# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading JSON, strictly: the grammar of RFC 8259 and nothing more.

"""
    JSONError(message, offset)

The text is not JSON. `offset` is the byte at which reading stopped, counted
from one.
"""
struct JSONError <: Exception
    message::String
    offset::Int
end

Base.showerror(io::IO, e::JSONError) = print(io, "JSON error at byte ", e.offset, ": ", e.message)

mutable struct Reader
    bytes::Vector{UInt8}
    position::Int
    depth::Int
    max_depth::Int
end

"""
    read_json(text; max_depth = 256)

Reads one JSON document. Objects become [`Object`](@ref), arrays `Vector{Any}`,
strings `String`, `true`/`false` `Bool`, and `null` `nothing`. A number
without a fraction or an exponent becomes an `Int64`, or a `BigInt` when it
does not fit, so integers of any size are read exactly; any other number
becomes a `Float64`.

The reader is strict: no comments, no trailing commas, no repeated member
names, no raw control characters in strings, nothing after the document.
Nesting deeper than `max_depth` is refused rather than followed.
"""
function read_json(text::AbstractString; max_depth::Int = 256)
    reader = Reader(Vector{UInt8}(codeunits(String(text))), 1, 0, max_depth)
    skip_space!(reader)
    value = read_value!(reader)
    skip_space!(reader)
    reader.position <= length(reader.bytes) && fail(reader, "unexpected text after the document")
    value
end

fail(reader::Reader, message::AbstractString) = throw(JSONError(String(message), reader.position))
at_end(reader::Reader) = reader.position > length(reader.bytes)
peek(reader::Reader) = at_end(reader) ? 0x00 : reader.bytes[reader.position]

function skip_space!(reader::Reader)
    while !at_end(reader) && reader.bytes[reader.position] in (0x20, 0x09, 0x0a, 0x0d)
        reader.position += 1
    end
end

function read_value!(reader::Reader)
    at_end(reader) && fail(reader, "the document ends where a value should start")
    byte = peek(reader)
    if byte == UInt8('{')
        read_object!(reader)
    elseif byte == UInt8('[')
        read_array!(reader)
    elseif byte == UInt8('"')
        read_string!(reader)
    elseif byte == UInt8('-') || UInt8('0') <= byte <= UInt8('9')
        read_number!(reader)
    elseif read_word!(reader, "true")
        true
    elseif read_word!(reader, "false")
        false
    elseif read_word!(reader, "null")
        nothing
    else
        fail(reader, "a value cannot start here")
    end
end

function read_word!(reader::Reader, word::String)
    stop = reader.position + ncodeunits(word) - 1
    (stop <= length(reader.bytes) && view(reader.bytes, reader.position:stop) == codeunits(word)) || return false
    reader.position = stop + 1
    true
end

function enter!(reader::Reader)
    reader.depth += 1
    reader.depth > reader.max_depth && fail(reader, "nesting is deeper than $(reader.max_depth) levels")
    reader.position += 1
    skip_space!(reader)
end

function read_object!(reader::Reader)
    enter!(reader)
    object = Object()
    if peek(reader) == UInt8('}')
        reader.position += 1
        reader.depth -= 1
        return object
    end
    while true
        peek(reader) == UInt8('"') || fail(reader, "a member name must be a string")
        name = read_string!(reader)
        haskey(object, name) && fail(reader, "the member \"$(name)\" appears twice")
        skip_space!(reader)
        peek(reader) == UInt8(':') || fail(reader, "a colon must follow a member name")
        reader.position += 1
        skip_space!(reader)
        object[name] = read_value!(reader)
        skip_space!(reader)
        byte = peek(reader)
        reader.position += 1
        if byte == UInt8('}')
            reader.depth -= 1
            return object
        elseif byte != UInt8(',')
            reader.position -= 1
            fail(reader, "a comma or a closing brace must follow a member")
        end
        skip_space!(reader)
    end
end

function read_array!(reader::Reader)
    enter!(reader)
    items = Any[]
    if peek(reader) == UInt8(']')
        reader.position += 1
        reader.depth -= 1
        return items
    end
    while true
        push!(items, read_value!(reader))
        skip_space!(reader)
        byte = peek(reader)
        reader.position += 1
        if byte == UInt8(']')
            reader.depth -= 1
            return items
        elseif byte != UInt8(',')
            reader.position -= 1
            fail(reader, "a comma or a closing bracket must follow an item")
        end
        skip_space!(reader)
    end
end

function read_hex4!(reader::Reader)
    reader.position + 3 <= length(reader.bytes) || fail(reader, "a \\u escape needs four hexadecimal digits")
    value = tryparse(UInt16, String(reader.bytes[reader.position:(reader.position + 3)]); base = 16)
    value === nothing && fail(reader, "a \\u escape needs four hexadecimal digits")
    reader.position += 4
    value
end

function read_string!(reader::Reader)
    reader.position += 1
    out = IOBuffer()
    while true
        at_end(reader) && fail(reader, "the document ends inside a string")
        byte = reader.bytes[reader.position]
        if byte == UInt8('"')
            reader.position += 1
            break
        elseif byte < 0x20
            fail(reader, "a control character must be escaped inside a string")
        elseif byte != UInt8('\\')
            write(out, byte)
            reader.position += 1
        else
            reader.position += 1
            at_end(reader) && fail(reader, "the document ends inside an escape")
            escape = reader.bytes[reader.position]
            reader.position += 1
            if escape == UInt8('u')
                unit = read_hex4!(reader)
                if 0xd800 <= unit <= 0xdbff
                    (peek(reader) == UInt8('\\') && reader.position < length(reader.bytes) &&
                     reader.bytes[reader.position + 1] == UInt8('u')) || fail(reader, "a high surrogate stands alone")
                    reader.position += 2
                    low = read_hex4!(reader)
                    0xdc00 <= low <= 0xdfff || fail(reader, "a high surrogate is not followed by a low one")
                    print(out, Char(0x10000 + ((UInt32(unit) - 0xd800) << 10) + (UInt32(low) - 0xdc00)))
                elseif 0xdc00 <= unit <= 0xdfff
                    fail(reader, "a low surrogate stands alone")
                else
                    print(out, Char(unit))
                end
            else
                plain = escape == UInt8('"') ? '"' : escape == UInt8('\\') ? '\\' : escape == UInt8('/') ? '/' :
                        escape == UInt8('n') ? '\n' : escape == UInt8('t') ? '\t' : escape == UInt8('r') ? '\r' :
                        escape == UInt8('b') ? '\b' : escape == UInt8('f') ? '\f' : nothing
                plain === nothing && (reader.position -= 1; fail(reader, "an unknown escape"))
                print(out, plain)
            end
        end
    end
    text = String(take!(out))
    isvalid(text) || fail(reader, "a string is not valid UTF-8")
    text
end

is_digit(byte::UInt8) = UInt8('0') <= byte <= UInt8('9')

function read_digits!(reader::Reader, what::String)
    start = reader.position
    while !at_end(reader) && is_digit(reader.bytes[reader.position])
        reader.position += 1
    end
    reader.position > start || fail(reader, "a digit must follow $(what)")
end

function read_number!(reader::Reader)
    start = reader.position
    peek(reader) == UInt8('-') && (reader.position += 1)
    if peek(reader) == UInt8('0')
        reader.position += 1
        is_digit(peek(reader)) && fail(reader, "a number cannot have a leading zero")
    else
        read_digits!(reader, "the sign")
    end
    integral = true
    if peek(reader) == UInt8('.')
        integral = false
        reader.position += 1
        read_digits!(reader, "the decimal point")
    end
    if peek(reader) in (UInt8('e'), UInt8('E'))
        integral = false
        reader.position += 1
        peek(reader) in (UInt8('+'), UInt8('-')) && (reader.position += 1)
        read_digits!(reader, "the exponent")
    end
    text = String(reader.bytes[start:(reader.position - 1)])
    if integral
        small = tryparse(Int64, text)
        small === nothing ? parse(BigInt, text) : small
    else
        parse(Float64, text)
    end
end
