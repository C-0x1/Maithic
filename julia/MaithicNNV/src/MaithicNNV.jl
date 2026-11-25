# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicNNV

Neural-network verification, the numeric side.

This package computes bounds on the outputs of ReLU networks quickly, in
floating point with outward rounding, and writes them down as *certificates*
with exact rational numbers. A certificate is not trusted because this package
produced it: it is checked by `Maithic.NN.checkCertificate` in Lean, a checker
that is proved sound against the exact semantics of the network. Julia
computes; the kernel decides.

The exact semantics implemented here in rational arithmetic are the same as
`Maithic.NN.Network.eval` in Lean, including the convention that a weight row
and an input of different lengths use their common prefix.
"""
module MaithicNNV

using MaithicCore: Object, document, exact_text, write_json, parse_exact, expect_schema, engine_record
using MaithicCore: exact as exact_value

export Activation, Identity, ReLU, Neuron, Layer, Network, Interval, Box
export exact, floating, evaluate, propagate, propagate_exact, enclose, Certificate, certify,
       check_certificate, certificate_document, certificate_json, certificate_lean, decide_property, Verdict, Verified, Violated,
       Unknown, lo, hi,
       encloses, width, is_subset, layer_boxes,
       network_from_document, property_from_document, network_document, verdict_document

const VERSION = v"0.1.0"

include("bounds/network.jl")
include("bounds/interval.jl")
include("bounds/propagate.jl")
include("bounds/certificate.jl")
include("bounds/decide.jl")
include("bounds/documents.jl")

end # module
