# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicCore

What the Maithic Julia packages share, with no dependency outside the standard
library:

- a JSON reader and writer whose output is deterministic, so that documents
  can be compared and hashed byte for byte;
- the lossless text form of integers and rationals used by every certificate
  (`"3"`, `"-7/2"`), because a certificate that went through a floating-point
  number is no longer a certificate;
- the small arithmetic language of the oracle exchange format, with a parser,
  printers for plain text and for Lean, and evaluation over any number type;
- the envelope that names the schema of a document;
- a pseudo-random generator whose streams are the same on every Julia version,
  so that seeded searches are reproducible for good.
"""
module MaithicCore

export Object, JSONError, read_json, write_json
export Exact, exact, exact_text, parse_exact
export Node, Num, Var, Neg, BinOp, Pow, Call, Relation, ParseError, UnboundVariable, InexactFunction
export parse_expression, parse_relation, parse_conjunction, expression_text, relation_text, variables,
       evaluate, holds, lift, substitute, negate
export document, schema_of, expect_schema, engine_record
export SplitMix, next_u64!, uniform!, integer!

const VERSION = v"0.1.0"

include("json/object.jl")
include("json/write.jl")
include("json/read.jl")
include("exact/numbers.jl")
include("expr/ast.jl")
include("expr/parse.jl")
include("expr/print.jl")
include("expr/eval.jl")
include("schema/envelope.jl")
include("random/splitmix.jl")

end # module
