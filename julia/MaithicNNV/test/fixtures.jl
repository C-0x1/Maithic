# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The network fixtures: certificates as JSON for the schema check, and as Lean
# source for the verified checker. The files live in fixtures/nnv and are
# shared with the other languages. The test suite fails when a file differs
# from what the code computes; `make julia-fixtures` rewrites them.

using MaithicNNV

const NNV_FIXTURE_ROOT = normpath(joinpath(@__DIR__, "..", "..", "..", "fixtures", "nnv"))

"|x| as a ReLU network, relu(x) + relu(-x), on [-2, 3]."
function absolute_value_certificate()
    net = Network(Layer([1 // 1; -1 // 1;;], [0 // 1, 0 // 1], ReLU), Layer([1 // 1 1 // 1], [0 // 1], Identity))
    certify(net, [Interval(-2 // 1, 3 // 1)])
end

"A network whose parameters are not exactly representable in binary, on the unit square."
function decimal_certificate()
    net = Network(Layer([0.1 -0.3; 0.7 0.2], [0.05, -0.15], ReLU), Layer([1.1 -0.9], [0.01], Identity))
    certify(net, [Interval(0.0, 1.0), Interval(0.0, 1.0)])
end

"Every fixture file as `relative path => content`, computed from scratch."
function nnv_fixtures()
    certificates = ["absolute-value" => absolute_value_certificate(), "decimal" => decimal_certificate()]
    files = Pair{String,String}[]
    lean = String["-- Network certificates for the verified checker.\n",
                  "-- Written by `make julia-fixtures` from julia/MaithicNNV; do not edit.\n\n",
                  "import Maithic.NN.Cert.Bounds\n\nopen Maithic.NN\n"]
    for (name, certificate) in certificates
        push!(files, "certificates/$(name).json" => certificate_json(certificate; pretty = true) * "\n")
        source = certificate_lean(certificate; name = replace(name, "-" => "_"))
        push!(lean, "\n", replace(source, "import Maithic.NN.Cert.Bounds\n\nopen Maithic.NN\n\n" => ""))
    end
    push!(files, "lean/Certificates.lean" => join(lean))
    files
end

if abspath(PROGRAM_FILE) == @__FILE__
    for (path, content) in nnv_fixtures()
        target = joinpath(NNV_FIXTURE_ROOT, path)
        mkpath(dirname(target))
        write(target, content)
    end
    println("wrote ", length(nnv_fixtures()), " files under ", NNV_FIXTURE_ROOT)
end
