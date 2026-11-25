# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Vector formats: JSON for contracts, little-endian binary for storage.

"""
    vector_json(v)

A vector as a JSON array. Each number is printed with the shortest text that
reads back to the same `Float64`.
"""
vector_json(v::AbstractVector{<:Real}) = string("[", join((repr(Float64(x)) for x in v), ","), "]")

"Reads a vector written by [`vector_json`](@ref)."
function parse_vector_json(text::AbstractString)
    body = strip(text)
    (startswith(body, "[") && endswith(body, "]")) || throw(ArgumentError("a vector must be a JSON array"))
    inner = strip(body[2:(end - 1)])
    isempty(inner) ? Float64[] : [parse(Float64, strip(part)) for part in split(inner, ",")]
end

const VECTOR_MAGIC = UInt8['M', 'V', 'E', 'C']

"""
    vector_bytes(v)

A compact binary form: the bytes `MVEC`, the length as a little-endian 32-bit
integer, then each coordinate as a little-endian 32-bit float.
"""
function vector_bytes(v::AbstractVector{<:Real})
    io = IOBuffer()
    write(io, VECTOR_MAGIC)
    write(io, htol(UInt32(length(v))))
    for x in v
        write(io, htol(Float32(x)))
    end
    take!(io)
end

"Reads a vector written by [`vector_bytes`](@ref)."
function parse_vector_bytes(bytes::AbstractVector{UInt8})
    (length(bytes) >= 8 && bytes[1:4] == VECTOR_MAGIC) || throw(ArgumentError("not a Maithic vector"))
    io = IOBuffer(bytes[5:end])
    n = Int(ltoh(read(io, UInt32)))
    length(bytes) == 8 + 4n || throw(ArgumentError("vector length does not match its header"))
    Float32[ltoh(read(io, Float32)) for _ in 1:n]
end
