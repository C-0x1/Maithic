# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicNNV
using MaithicNNV: Exact, final_box, input_size, output_size
import MaithicCore
using MaithicCore: read_json

# |x| as a ReLU network: relu(x) + relu(-x). The same network is used by the
# Lean tests in lean/MaithicTest/NN.lean, with the same expected numbers.
abs_net() = Network(Layer([1 // 1; -1 // 1;;], [0 // 1, 0 // 1], ReLU), Layer([1 // 1 1 // 1], [0 // 1], Identity))

# A network with parameters that are not exactly representable in binary.
decimal_net() = Network(Layer([0.1 -0.3; 0.7 0.2], [0.05, -0.15], ReLU), Layer([1.1 -0.9], [0.01], Identity))

include("fixtures.jl")

q(x) = Exact(x)
ival(a, b) = Interval(q(a), q(b))

@testset "MaithicNNV" begin
    @testset "P08-03-01 network structs hold exact and floating parameters" begin
        net = abs_net()
        @test net isa Network{Rational{Int}}
        @test exact(net) isa Network{Exact}
        @test floating(net) isa Network{Float64}
        @test input_size(net) == 1 && output_size(net) == 1
        @test length(net.layers[1].neurons) == 2
        @test net.layers[1].activation == ReLU && net.layers[2].activation == Identity
        # Converting a floating-point parameter to exact form loses nothing.
        @test exact(decimal_net()).layers[1].neurons[1].weights[1] == Exact(0.1)
        @test Exact(0.1) != 1 // 10
        @test_throws DimensionMismatch Layer([1 2; 3 4], [1], ReLU)
    end

    @testset "P08-03-05 the reference forward pass is exact" begin
        net = exact(abs_net())
        @test evaluate(net, [q(-5 // 2)]) == [q(5 // 2)]
        @test evaluate(net, [q(3)]) == [q(3)]
        @test evaluate(net, [q(0)]) == [q(0)]
        @test evaluate(Network{Exact}(Layer{Exact}[]), [q(1), q(2)]) == [q(1), q(2)]
        affine = Network(Layer([2 // 1 -1 // 1; 1 // 1 1 // 1], [1 // 2, -1 // 1], Identity))
        @test evaluate(exact(affine), [q(3), q(4)]) == [q(5 // 2), q(6)]
        # A row longer than the input uses the common prefix, as in Lean.
        @test evaluate(exact(affine), [q(3)]) == [q(13 // 2), q(2)]
        @test evaluate(floating(abs_net()), [-2.5]) == [2.5]
    end

    @testset "P08-03-02 interval propagation rounds outwards and stays sound" begin
        net = decimal_net()
        input = [Interval(-1.0, 1.0), Interval(0.0, 2.0)]
        fast = propagate(net, input)
        slow = propagate_exact(net, input)
        @test length(fast) == length(slow) == 2
        # The floating-point bounds enclose the exact bounds, layer by layer ...
        @test all(is_subset(slow[k], fast[k]) for k in eachindex(fast))
        # ... are strictly wider somewhere, because rounding really happened ...
        @test any(fast[k] != slow[k] for k in eachindex(fast))
        # ... and only by a few units in the last place.
        @test all(width(f) - width(s) < 1 // 10^12 for (fb, sb) in zip(fast, slow) for (f, s) in zip(fb, sb))
        # Soundness on sample points, in exact arithmetic.
        exact_net = exact(net)
        for a in -1:1//2:1, b in 0:1//2:2
            @test encloses(last(fast), evaluate(exact_net, [q(a), q(b)]))
        end
    end

    @testset "P08-03-02 exact propagation matches the Lean checker on the shared example" begin
        boxes = propagate_exact(abs_net(), [ival(-2, 3)])
        @test boxes == [[ival(0, 3), ival(0, 2)], [ival(0, 5)]]
        @test propagate(abs_net(), [ival(-2, 3)])[end][1].lo <= 0
        @test propagate(abs_net(), [ival(-2, 3)])[end][1].hi >= 5
    end

    @testset "P08-03-06 floating bounds become enclosing rationals" begin
        i = enclose(Interval(0.1, 0.3))
        @test i isa Interval{Exact}
        @test i.lo == Exact(0.1) && i.hi == Exact(0.3)
        @test i.lo > 1 // 10          # 0.1 is slightly above one tenth
        @test MaithicNNV.float_down(q(1 // 10)) < 0.1 || Exact(MaithicNNV.float_down(q(1 // 10))) <= 1 // 10
        @test Exact(MaithicNNV.float_down(q(1 // 3))) <= 1 // 3 <= Exact(MaithicNNV.float_up(q(1 // 3)))
        @test MaithicNNV.float_down(q(1 // 2)) == MaithicNNV.float_up(q(1 // 2)) == 0.5
        @test_throws ArgumentError enclose(Interval(0.0, Inf))
        @test_throws ArgumentError Interval(2.0, 1.0)
        @test_throws ArgumentError Interval(NaN, 1.0)
    end

    @testset "P08-03-03 certificates are written with exact rationals" begin
        c = certify(abs_net(), [ival(-2, 3)])
        @test check_certificate(c)
        @test is_subset([ival(0, 5)], final_box(c))
        json = certificate_json(c)
        @test startswith(json, "{\"schema\":\"maithic.nnv.certificate/1\"")
        @test occursin("\"input\":[[\"-2\",\"3\"]]", json)
        @test occursin("\"activation\":\"relu\"", json)
        @test occursin("{\"weights\":[\"-1\"],\"bias\":\"0\"}", json)
        @test !occursin(r"\d\.\d", json) && !occursin(r"\de[-+]?\d", json)   # no floating-point literal anywhere
        exact_cert = Certificate(exact(abs_net()), [ival(-2, 3)], propagate_exact(abs_net(), [ival(-2, 3)]))
        @test occursin("\"boxes\":[[[\"0\",\"3\"],[\"0\",\"2\"]],[[\"0\",\"5\"]]]", certificate_json(exact_cert))
        lean = certificate_lean(exact_cert; name = "absCert")
        @test occursin("def absCertNet : Network := [", lean)
        @test occursin("{ neurons := [⟨[1], 0⟩, ⟨[(-1)], 0⟩], activation := .relu }", lean)
        @test occursin("def absCertInput : Box := [⟨(-2), 3⟩]", lean)
        @test occursin("#guard checkCertificate absCertNet absCertInput absCert", lean)
    end

    @testset "P08-03-03 a certificate that claims too much is rejected" begin
        net, input = exact(abs_net()), [ival(-2, 3)]
        @test check_certificate(Certificate(net, input, [[ival(0, 3), ival(0, 2)], [ival(0, 11 // 2)]]))
        @test !check_certificate(Certificate(net, input, [[ival(0, 2), ival(0, 2)], [ival(0, 11 // 2)]]))
        @test !check_certificate(Certificate(net, input, [[ival(0, 3), ival(0, 2)], [ival(0, 4)]]))
        @test !check_certificate(Certificate(net, input, [[ival(0, 3), ival(0, 2)]]))
        @test !check_certificate(Certificate(net, input, [[ival(0, 3)], [ival(0, 5)]]))
    end

    @testset "P08-03-04 a property is verified, violated or unknown" begin
        net, input = abs_net(), [ival(-2, 3)]
        verdict, evidence = decide_property(net, input, [ival(-1, 6)])
        @test verdict == Verified && evidence isa Certificate && check_certificate(evidence)
        verdict, point = decide_property(net, input, [ival(0, 2)])
        @test verdict == Violated
        @test encloses([ival(-2, 3)], point) && !encloses([ival(0, 2)], evaluate(exact(net), point))
        # True on the whole box (the real range is [0, 3]) but out of reach of interval propagation.
        verdict, nothing_found = decide_property(net, input, [ival(0, 4)])
        @test verdict == Unknown && nothing_found === nothing
    end

    @testset "P08-03-04 bounds that rounding makes too loose are computed again exactly" begin
        net, input = abs_net(), [ival(-2, 3)]
        # In floating point the lower bound of the output is a hair below zero; exactly, it is zero.
        verdict, evidence = decide_property(net, input, [ival(0, 5)])
        @test verdict == Verified && check_certificate(evidence)
        @test evidence.boxes == propagate_exact(net, input)
        # Without the exact attempt the same property stays open.
        verdict, _ = decide_property(net, input, [ival(0, 5)]; exact_limit = 0)
        @test verdict == Unknown
    end

    @testset "P08-03-04 networks and properties are read from documents, and a verdict is written as one" begin
        example(name) = read_json(read(joinpath(@__DIR__, "..", "..", "..", "examples", "nnv", name), String))
        net = network_from_document(example("absolute-value.json"))
        @test net isa Network{MaithicNNV.Exact} && length(net.layers) == 2
        @test evaluate(net, MaithicNNV.Exact[-2]) == [2]
        @test network_document(net)["layers"] == example("absolute-value.json")["layers"]
        classifier = network_from_document(example("small-classifier.json"))
        @test classifier.layers[1].neurons[1].weights == [1 // 10, -3 // 10]   # decimals are read exactly
        input, claimed = property_from_document(example("bounded.json"))
        @test input == [ival(-2, 3)] && claimed == [ival(0, 6)]

        holds = verdict_document(net, input, claimed)
        @test holds["schema"] == "maithic.nnv.verdict/1" && holds["verdict"] == "verified"
        @test holds["boxes"] == Any[Any[Any["0", "3"], Any["0", "2"]], Any[Any["0", "5"]]]
        fails = verdict_document(net, property_from_document(example("too-tight.json"))...)
        @test fails["verdict"] == "violated" && fails["counterexample"]["point"] == ["3"] && fails["counterexample"]["output"] == ["3"]
        open_ = verdict_document(net, property_from_document(example("beyond-the-bounds.json"))...)
        @test open_["verdict"] == "unknown" && occursin("too loose", open_["reason"])
        @test !haskey(open_, "boxes") && !haskey(open_, "counterexample")

        # What is not a network or a property is refused with a reason.
        @test_throws ArgumentError network_from_document(example("bounded.json"))
        @test_throws ArgumentError property_from_document(example("absolute-value.json"))
        bad(layers) = MaithicCore.document("maithic.nnv.network/1", "layers" => layers)
        neuron(weights...) = MaithicCore.Object("weights" => Any[weights...], "bias" => "0")
        layer(neurons...; activation = "relu") = MaithicCore.Object("activation" => activation, "neurons" => Any[neurons...])
        @test_throws ArgumentError network_from_document(bad(Any[]))
        @test_throws ArgumentError network_from_document(bad(Any[layer(neuron("1"); activation = "tanh")]))
        @test_throws ArgumentError network_from_document(bad(Any[layer(neuron("1"), neuron("1", "2"))]))
        @test_throws ArgumentError network_from_document(bad(Any[layer(neuron("1")), layer(neuron("1", "2"))]))
        @test_throws ArgumentError network_from_document(bad(Any[layer(neuron("one"))]))
        @test network_from_document(bad(Any[layer(neuron(0.5))])).layers[1].neurons[1].weights == [1 // 2]
        box(pairs...) = MaithicCore.document("maithic.nnv.property/1", "input" => Any[pairs...], "output" => Any[Any["0", "1"]])
        @test_throws ArgumentError property_from_document(box(Any["1", "0"]))
        @test_throws ArgumentError property_from_document(box(Any["0"]))
        @test_throws ArgumentError verdict_document(net, [ival(0, 1), ival(0, 1)], [ival(0, 1)])
        @test_throws ArgumentError verdict_document(net, [ival(0, 1)], [ival(0, 1), ival(0, 1)])
    end

    @testset "P08-03-07 bounds are deterministic" begin
        net = decimal_net()
        input = [Interval(-1.0, 1.0), Interval(0.0, 2.0)]
        @test propagate(net, input) == propagate(net, input)
        @test certificate_json(certify(net, input)) == certificate_json(certify(net, input))
        @test hash(Interval(1, 2)) == hash(Interval(1, 2))
    end
    @testset "P08-03-03 the shared fixtures are what the code computes" begin
        files = nnv_fixtures()
        @test first.(files) == ["certificates/absolute-value.json", "certificates/decimal.json", "lean/Certificates.lean"]
        for (path, content) in files
            target = joinpath(NNV_FIXTURE_ROOT, path)
            @test isfile(target) && read(target, String) == content
        end
        @test check_certificate(absolute_value_certificate()) && check_certificate(decimal_certificate())
        @test occursin("\"schema\": \"maithic.nnv.certificate/1\"", files[1][2])
        @test count("#guard checkCertificate", files[3][2]) == 2
    end
end
