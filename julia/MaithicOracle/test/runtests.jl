# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicCore
using MaithicOracle

include("fixtures.jl")

"Reads the certificate back from its JSON and checks that nothing was lost."
function round_trip(certificate)
    text = certificate_json(certificate)
    reread = read_certificate(text)
    check(reread) && certificate_json(reread) == text && kind(reread) == kind(certificate)
end

@testset "MaithicOracle" begin
    include("discrete.jl")
    include("linear.jl")
    include("poly.jl")
    include("interval.jl")

    @testset "the shared fixtures are what the code computes" begin
        files = oracle_fixtures()
        @test length(files) == 2 * length(GOAL_FIXTURES) + length(discrete_fixtures()) + 2
        for (path, content) in files
            target = joinpath(FIXTURE_ROOT, path)
            @test isfile(target) && read(target, String) == content
        end
        on_disk = String[]                                          # in the directories this package writes
        for directory in WRITTEN_DIRECTORIES, (root, _, names) in walkdir(joinpath(FIXTURE_ROOT, directory)), name in names
            push!(on_disk, relpath(joinpath(root, name), FIXTURE_ROOT))
        end
        @test sort(on_disk) == sort(first.(files))                  # no file is left over from an earlier suite
        outcomes = Dict(name => read_json(read(joinpath(FIXTURE_ROOT, "outcomes", "$(name).json"), String))["outcome"]
                        for (name, _) in GOAL_FIXTURES)
        @test all(outcomes[name] == (endswith(name, "refuted") ? "refuted" : endswith(name, "unknown") ? "unknown" : "proved")
                  for (name, _) in GOAL_FIXTURES)
        replayed = read(joinpath(FIXTURE_ROOT, "lean", "Replay.lean"), String)
        @test count("\ntheorem replay_", replayed) == count(((name, _),) -> startswith(name, "linear-integer"), GOAL_FIXTURES)
        for (name, _) in discrete_fixtures()
            certificate = read_certificate(read(joinpath(FIXTURE_ROOT, "certificates", "$(name).json"), String))
            @test check(certificate)
        end
        for (name, goal) in GOAL_FIXTURES
            reread = read_goal(read(joinpath(FIXTURE_ROOT, "goals", "$(name).json"), String))
            @test goal_document(reread) == goal_document(goal)      # 1.8 is written as 9 / 5, which reads back as a quotient
            outcomes[name] == "proved" || continue
            doc = read_json(read(joinpath(FIXTURE_ROOT, "outcomes", "$(name).json"), String))
            @test check(read_certificate(doc["certificate"]))
        end
    end
end
