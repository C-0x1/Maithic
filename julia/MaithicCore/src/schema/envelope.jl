# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Every document that crosses a language boundary names its schema first.

"""
    document(schema, members...)

An [`Object`](@ref) whose first member is `"schema"`, followed by the given
members in order. Schema names have the form `maithic.<area>.<kind>/<major>`,
and the schemas themselves are in the `schemas/` folder of the repository.
"""
document(schema::AbstractString, members::Pair...) = Object("schema" => String(schema), members...)

"The schema a document names, or `nothing`."
schema_of(value) = value isa Object ? get(value, "schema", nothing) : nothing

"""
    expect_schema(value, schema)

Returns the document if it names `schema`, and otherwise throws an
`ArgumentError` that says what was found.
"""
function expect_schema(value, schema::AbstractString)
    found = schema_of(value)
    found == schema && return value
    throw(ArgumentError(found === nothing ? "the document names no schema; \"$(schema)\" is expected" :
                        "the document has schema \"$(found)\"; \"$(schema)\" is expected"))
end

"""
    engine_record(name, version, parameters...)

The provenance member of a certificate: which engine produced it, at which
version and with which parameters. A checker never reads it; a person
investigating a result does.
"""
engine_record(name::AbstractString, version, parameters::Pair...) =
    Object("name" => String(name), "version" => string(version), "parameters" => Object(parameters...))
