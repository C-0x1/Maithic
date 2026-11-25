# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading certificates back, whatever their kind.

"The function that rebuilds a certificate of each kind from its data."
const READERS = Dict{String,Function}()

"The certificate kinds this package reads and writes, sorted."
certificate_kinds() = sort!(collect(keys(READERS)))

"""
    read_certificate(text)

Reads a certificate document of the schema `maithic.oracle.certificate/1`
and returns the certificate, ready for [`check`](@ref). Reading validates
the shape of the data, not the fact: a well-formed certificate of a false
claim is read without complaint and then fails its check. A document of the
wrong shape is an `ArgumentError`.
"""
read_certificate(text::AbstractString) = read_certificate(read_json(text))

function read_certificate(doc)
    expect_schema(doc, CERTIFICATE_SCHEMA)
    name = get(doc, "kind", nothing)
    (name isa AbstractString && haskey(READERS, name)) ||
        throw(ArgumentError("unknown certificate kind $(name === nothing ? "(none)" : repr(name))"))
    data = get(doc, "data", nothing)
    data isa Object || throw(ArgumentError("a $(name) certificate has no data"))
    try
        READERS[name](data)
    catch e
        e isa InterruptException && rethrow()
        throw(ArgumentError("malformed $(name) certificate: $(sprint(showerror, e))"))
    end
end
