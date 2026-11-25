# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicGraphs
using MaithicCore

const ROOT = normpath(joinpath(@__DIR__, "..", "..", ".."))
fixture(parts...) = read(joinpath(ROOT, "fixtures", parts...), String)

# a uses b, b uses c: a chain.
chain() = CodeGraph(["a", "b", "c"], ["a" => "b", "b" => "c"])
# Three lemmas that all use one definition, which uses nothing.
star() = CodeGraph(["hub", "x", "y", "z"], ["x" => "hub", "y" => "hub", "z" => "hub"])

@testset "MaithicGraphs" begin
    @testset "P07-04-01 a graph is loaded into adjacency lists" begin
        g = chain()
        @test node_count(g) == 3 && edge_count(g) == 2
        @test g.uses == [[2], [3], Int[]]
        @test g.used_by == [Int[], [1], [2]]
        # Repeated edges count once, and a node does not use itself.
        doubled = CodeGraph(["a", "b"], ["a" => "b", "a" => "b", "a" => "a"])
        @test doubled.uses == [[2], Int[]]
        @test_throws ArgumentError CodeGraph(["a"], ["a" => "nowhere"])
        @test_throws ArgumentError CodeGraph(["a", "a"], Pair{String,String}[])

        text = fixture("intel", "graph.json")
        doc = read_json(text)
        g = load_graph(text)
        declared = [n for n in doc["nodes"] if n["kind"] == "declaration"]
        @test node_count(g) == length(declared)
        @test edge_count(g) == count(e -> e["kind"] == "uses", doc["edges"])
        @test count(g.internal) == count(n -> n["internal"], declared)
        @test g.kinds[g.index["double_eq"]] == "theorem"
        @test g.index["open_one"] in g.uses[g.index["uses_open"]]
        # Without what comes from outside, only edges between the scanned declarations are left.
        inside = load_graph(text; internal_only = true)
        @test node_count(inside) == count(g.internal) && all(inside.internal)
        @test !haskey(inside.index, "Nat.add_comm") && haskey(g.index, "Nat.add_comm")
        @test_throws ArgumentError load_graph("""{"schema": "maithic.intel.health/1"}""")
        @test_throws ArgumentError load_graph("[1, 2]")
        # From a file, as a scan leaves it.
        path = joinpath(ROOT, "fixtures", "intel", "graph.json")
        from_file = read_graph(path)
        @test from_file.names == load_graph(read(path, String)).names
        @test node_count(read_graph(path; internal_only = true)) == count(from_file.internal)
        @test_throws ArgumentError read_graph(joinpath(ROOT, "fixtures", "intel", "no-such-graph.json"))
    end

    @testset "P07-04-02 degrees, PageRank and betweenness" begin
        g = chain()
        @test in_degree(g) == [0, 1, 1] && out_degree(g) == [1, 1, 0]
        # Only b lies between two others.
        @test betweenness(g) == [0.0, 1.0, 0.0]
        rank = pagerank(g)
        @test sum(rank) ≈ 1.0 && all(>(0), rank)
        @test rank[3] > rank[2] > rank[1]

        s = star()
        @test in_degree(s) == [3, 0, 0, 0]
        rank = pagerank(s)
        @test argmax(rank) == 1 && rank[2] ≈ rank[3] ≈ rank[4]
        @test betweenness(s) == zeros(4)
        # Two ways of the same length share the count: a reaches d through b and through c.
        diamond = CodeGraph(["a", "b", "c", "d"], ["a" => "b", "a" => "c", "b" => "d", "c" => "d"])
        @test betweenness(diamond) == [0.0, 0.5, 0.5, 0.0]
        # Two declarations that use each other rank alike.
        @test pagerank(CodeGraph(["a", "b"], ["a" => "b", "b" => "a"])) ≈ [0.5, 0.5]
        # With no damping left, every declaration ranks the same.
        @test pagerank(chain(); damping = 0.0) ≈ fill(1 / 3, 3)
        @test pagerank(CodeGraph(String[], Pair{String,String}[])) == Float64[]
        @test_throws ArgumentError pagerank(g; damping = 1.0)
        @test pagerank(g) == pagerank(g)

        @test top(s, in_degree(s), 2) == ["hub" => 3, "x" => 0]
        @test_throws ArgumentError top(s, [1, 2])

        real = load_graph(fixture("intel", "graph.json"))
        used = in_degree(real)
        @test first(top(real, used, 1; internal_only = true)) == ("Maithic.Algo.Sorted" => 12)
        rank = pagerank(real)
        @test sum(rank) ≈ 1.0
        # What everything is built from ranks above a result that nothing uses.
        @test rank[real.index["Nat"]] > rank[real.index["by_shortcut"]]
        @test all(>=(0), betweenness(real))
    end

    @testset "P07-04-03 weak components and their sizes" begin
        @test weak_components(chain()) == [[1, 2, 3]]
        apart = CodeGraph(["a", "b", "c", "d"], ["a" => "b", "c" => "b"])
        @test weak_components(apart) == [[1, 2, 3], [4]]
        @test component_sizes(apart) == [3, 1]
        # The direction of an edge does not matter, and equal sizes keep the order of the nodes.
        @test weak_components(CodeGraph(["a", "b", "c", "d"], ["b" => "a", "d" => "c"])) == [[1, 2], [3, 4]]
        @test weak_components(CodeGraph(String[], Pair{String,String}[])) == Vector{Int}[]

        real = load_graph(fixture("intel", "graph.json"))
        sizes = component_sizes(real)
        @test sum(sizes) == node_count(real) && issorted(sizes; rev = true)
        # The demonstration file and the library files both rest on the core of Lean.
        @test first(sizes) > node_count(real) ÷ 2
    end

    @testset "P07-04-02 the measures of a graph are one document, kept in the repository" begin
        real = load_graph(fixture("intel", "graph.json"))
        doc = metrics(real)
        @test doc["schema"] == METRICS_SCHEMA
        @test doc["nodes"] == node_count(real) && doc["edges"] == edge_count(real)
        @test length(doc["declarations"]) == count(real.internal)
        ranks = [d["pagerank"] for d in doc["declarations"]]
        @test issorted(ranks; rev = true)
        @test doc["components"]["sizes"] == component_sizes(real)
        @test read_json(fixture("intel", "metrics.json")) == read_json(write_json(doc))
        # The program that the Makefile runs writes the same document.
        script = joinpath(@__DIR__, "..", "bin", "metrics.jl")
        out = read(`$(Base.julia_cmd()) --startup-file=no --project=$(dirname(@__DIR__)) $script $(joinpath(ROOT, "fixtures", "intel", "graph.json"))`, String)
        @test read_json(out) == read_json(fixture("intel", "metrics.json"))
    end
end
