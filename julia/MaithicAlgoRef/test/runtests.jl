# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicAlgoRef
using MaithicAlgoRef: agrees, is_path_answer, some_path
using MaithicCore: SplitMix

const ALGO_EXECUTABLE = joinpath(@__DIR__, "..", "..", "..", "lean", ".lake", "build", "bin", "maithic-algo")

@testset "MaithicAlgoRef" begin
    @testset "P05-05-01 every algorithm of the Lean library has a reference" begin
        @test length(reference_names()) == 19 && allunique(reference_names())
        @test reference("insertion_sort", [3, 1, 2, 1]) == [1, 1, 2, 3]
        @test all(reference(name, [5, -1, 3, 3]) == [-1, 3, 3, 5] for name in ("merge_sort", "selection_sort", "quick_sort"))
        @test reference("linear_search", 2, [3, 1, 2, 2]) == 2 && reference("linear_search", 9, [3, 1]) === nothing
        @test reference("binary_search", 2, [1, 2, 3]) && !reference("binary_search", 9, [1, 2, 3])
        @test reference("minimum", [4, 2, 8]) == 2 && reference("maximum", [4, 2, 8]) == 8 && reference("minimum", Int[]) === nothing
        @test reference("dedup_sorted", [1, 1, 2, 3, 3]) == [1, 2, 3]
        @test reference("is_sorted", [1, 2, 2]) && !reference("is_sorted", [2, 1])
        @test reference("same_counts", [1, 2, 2], [2, 1, 2]) && !reference("same_counts", [1, 2], [1, 1])
        @test reference("fib", 30) == 832040 && reference("fib", 0) == 0 && reference("fib", 200) == big"280571172992510140037611932413038677189525"
        @test reference("prefix_sums", [1, 2, 3]) == [0, 1, 3, 6] && reference("prefix_sums", Int[]) == [0]
        @test reference("range_sum", 1, 3, [5, 6, 7, 8]) == 13 && reference("range_sum", 2, 2, [5, 6, 7]) == 0
        @test reference("gcd", 12, 18) == 6 && reference("gcd", 0, 7) == 7 && reference("gcd", 0, 0) == 0
        @test reference("pow", 3, 5) == 243 && reference("pow", 2, 100) == big(2)^100 && reference("pow", 0, 0) == 1
        @test reference("pow_mod", 3, 200, 13) == 9 && reference("pow_mod", 2, 1000, 1) == 0 && reference("pow_mod", 5, 0, 7) == 1
        @test reference("find_sub", [2, 3], [1, 2, 3, 2, 3]) == 1 && reference("find_sub", [3, 3], [1, 2, 3]) === nothing
        @test reference("find_sub", Int[], [1, 2]) == 0 && reference("find_sub", [1], Int[]) === nothing
        square = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (4, 0)]
        @test reference("path", 5, 0, 3, square) && reference("path", 5, 2, 2, square) && !reference("path", 5, 0, 4, square)
        @test_throws ArgumentError reference("shuffle", [1, 2])
    end

    @testset "P05-05-02 inputs are drawn from a seed, within the size that is asked for" begin
        draw(f, seed = 7) = (rng = SplitMix(UInt64(seed)); [f(rng) for _ in 1:200])
        lists = draw(rng -> random_list(rng, 9))
        @test lists == draw(rng -> random_list(rng, 9))                      # the same seed, the same inputs
        @test lists != draw(rng -> random_list(rng, 9), 8)                   # another seed, other inputs
        @test all(length(l) <= 9 && all(-9 <= x <= 9 for x in l) for l in lists)
        @test any(isempty, lists) && any(l -> length(l) == 9, lists)         # the whole range of lengths occurs
        @test all(all(2 <= x <= 3 for x in l) for l in draw(rng -> random_list(rng, 5; lo = 2, hi = 3)))
        @test all(issorted, draw(rng -> random_sorted_list(rng, 9)))
        @test all(a -> length(a) == 6, draw(rng -> random_array(rng, 6)))
        graphs = draw(rng -> random_graph(rng, 6))
        @test all(1 <= n <= 6 && allunique(edges) && all(0 <= u < n && 0 <= v < n for (u, v) in edges) for (n, edges) in graphs)
        @test all(isempty(edges) for (_, edges) in draw(rng -> random_graph(rng, 5; density = 0)))
        @test all(length(edges) == n^2 for (n, edges) in draw(rng -> random_graph(rng, 5; density = 1)))
        @test adjacency(3, [(0, 1), (0, 2), (2, 0)]) == Dict(0 => [1, 2], 1 => Int[], 2 => [0])
        texts = draw(rng -> random_string(rng, 8))
        @test all(length(t) <= 8 && all(c in "ab" for c in t) for t in texts) && any(t -> occursin("ab", t), texts)
        @test all(all(c in "xyz" for c in t) for t in draw(rng -> random_string(rng, 8; alphabet = "xyz")))
        @test_throws ArgumentError random_list(SplitMix(UInt64(1)), -1)
        @test_throws ArgumentError random_list(SplitMix(UInt64(1)), 3; lo = 2, hi = 1)
        @test_throws ArgumentError random_graph(SplitMix(UInt64(1)), 0)
        @test_throws ArgumentError random_graph(SplitMix(UInt64(1)), 3; density = 2)
        @test_throws ArgumentError random_string(SplitMix(UInt64(1)), 3; alphabet = "")
    end

    @testset "P05-05-03 cases are requests with what makes an answer right" begin
        batch = cases(; seed = 3, count = 190, size = 10)
        @test length(batch) == 190 && [c.algorithm for c in batch[1:19]] == reference_names()
        @test [c.request for c in batch] == [c.request for c in cases(; seed = 3, count = 190, size = 10)]
        @test all(c -> startswith(c.request, c.algorithm), batch)
        @test all(c -> c.accept(c.sample), batch)                           # the reference accepts its own answer
        @test all(c -> !c.accept("error: nothing"), batch)
        # An algorithm with several right answers is judged by what makes an answer right.
        searches = filter(c -> c.algorithm == "binary_search" && c.sample != "none", cases(; seed = 5, count = 400, size = 12, algorithms = ["binary_search"]))
        @test !isempty(searches) && all(c -> !c.accept("none") && !c.accept("-1") && !c.accept("99"), searches)
        @test any(c -> any(c.accept(string(i)) for i in 0:12 if string(i) != c.sample), searches)   # another position that holds the target is right too
        square = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (4, 0)]
        @test some_path(5, square, 0, 3) == [0, 2, 3] && some_path(5, square, 0, 4) === nothing && some_path(5, square, 1, 1) == [1]
        @test is_path_answer("0 1 2 3", square, 0, 3) && is_path_answer("0 2 3", square, 0, 3)
        @test !is_path_answer("0 3", square, 0, 3) && !is_path_answer("1 2 3", square, 0, 3) && !is_path_answer("none", square, 0, 3) && !is_path_answer("", square, 0, 3)
        @test_throws ArgumentError cases(; algorithms = ["shuffle"])
        @test_throws ArgumentError cases(; algorithms = String[])
        @test_throws ArgumentError make_case("fib", SplitMix(UInt64(1)), 0)
    end

    @testset "P05-05-03 the differential runner reports every answer the reference does not accept" begin
        clean = run_differential(reference_transport(); seed = 2, count = 190, size = 10)
        @test agrees(clean) && clean.cases == 190 && all(n == 10 && bad == 0 for (_, n, bad) in clean.by_algorithm)

        # An implementation whose sorts drop repeats, and whose searches are off by one.
        function faulty(batch)
            map(batch) do c
                c.algorithm == "quick_sort" ? join(unique(parse.(Int, split(c.sample))), " ") :
                c.algorithm == "linear_search" && c.sample != "none" ? string(parse(Int, c.sample) + 1) : c.sample
            end
        end
        report = run_differential(faulty; seed = 2, count = 380, size = 10)
        @test !agrees(report)
        wrong = Dict(name => bad for (name, _, bad) in report.by_algorithm)
        @test wrong["quick_sort"] > 0 && wrong["linear_search"] > 0
        @test all(bad == 0 for (name, bad) in wrong if !(name in ("quick_sort", "linear_search")))
        @test all(m -> m.case.algorithm in ("quick_sort", "linear_search") && !m.case.accept(m.answer), report.mismatches)

        doc = report_document(report; implementation = "a faulty one")
        @test doc["schema"] == "maithic.algolab.differential/1" && doc["cases"] == 380 && !doc["agrees"]
        @test doc["mismatches"] == length(report.mismatches) == length(doc["failures"])
        failure = first(doc["failures"])
        @test all(haskey(failure, key) for key in ("algorithm", "request", "expected", "answer")) && failure["expected"] != failure["answer"]
        @test report_document(clean)["agrees"] && isempty(report_document(clean)["failures"])

        # A transport is a process: requests go in one to a line, and answers come back one to a line.
        echoed = process_transport(`cat`)(cases(; count = 5))
        @test echoed == [c.request for c in cases(; count = 5)]
        @test_throws ArgumentError process_transport(`true`)(cases(; count = 5))
        @test_throws ArgumentError run_differential(batch -> String[]; count = 5)
        @test process_transport(`cat`)(MaithicAlgoRef.Case[]) == String[]
    end

    @testset "P05-05-03 the compiled Lean algorithms agree with the references" begin
        if isfile(ALGO_EXECUTABLE)
            report = run_differential(process_transport(Cmd([ALGO_EXECUTABLE])); seed = 1, count = 380, size = 12)
            @test agrees(report) && report.cases == 380
            big_inputs = run_differential(process_transport(Cmd([ALGO_EXECUTABLE])); seed = 4, count = 190, size = 40)
            @test agrees(big_inputs)
        else
            @test_skip "the Lean executable is not built; `make build-lean` builds it"
        end
    end

    @testset "costs: requests, proved bounds and the table" begin
        @test cost_names() == ["linear_search", "binary_search", "insertion_sort", "merge_sort"]
        @test [MaithicAlgoRef.floor_log2(n) for n in (0, 1, 2, 3, 4, 7, 8, 1023, 1024)] == [0, 0, 1, 1, 2, 2, 3, 9, 10]
        @test proven_bound("linear_search", 100) == 100
        @test proven_bound("binary_search", 100) == 7
        @test proven_bound("insertion_sort", 100) == 4950
        @test proven_bound("merge_sort", 100) == 700
        @test [proven_bound(a, 0) for a in cost_names()] == [0, 1, 0, 0]
        @test_throws ArgumentError proven_bound("quick_sort", 8)
        @test_throws ArgumentError proven_bound("merge_sort", -1)
        @test cost_request("linear_search", 3) == "cost linear_search 0 | 1 2 3"
        @test cost_request("binary_search", 3) == "cost binary_search 4 | 1 2 3"
        @test cost_request("insertion_sort", 4) == "cost insertion_sort 4 3 2 1"
        @test startswith(cost_request("merge_sort", 4), "cost merge_sort ")
        @test_throws ArgumentError cost_request("fib", 4)
        for n in (0, 1, 2, 17, 512)
            @test sort(MaithicAlgoRef.scrambled(n)) == collect(1:n)
        end
        @test MaithicAlgoRef.scrambled(64) == MaithicAlgoRef.scrambled(64) && !issorted(MaithicAlgoRef.scrambled(64))
        # an implementation that always reaches its bound
        at_bound = lines -> map(lines) do line
            words = split(line)
            n = occursin("|", line) ? length(split(split(line, "|")[end])) : length(words) - 2
            string(proven_bound(String(words[2]), n))
        end
        rows = cost_table(at_bound; sizes = [4, 8])
        @test length(rows) == 8 && all(r -> r.comparisons == r.bound, rows) && within_bounds(rows)
        @test [(r.algorithm, r.n) for r in rows][1:3] == [("linear_search", 4), ("linear_search", 8), ("binary_search", 4)]
        csv = cost_csv(rows)
        @test startswith(csv, "algorithm,n,comparisons,bound\nlinear_search,4,4,4\n") && count(==('\n'), csv) == 9
        @test !within_bounds([CostRow("linear_search", 4, 5, 4)])
        @test_throws ArgumentError cost_table(lines -> String["many" for _ in lines]; sizes = [4])
        @test_throws ArgumentError cost_table(lines -> String[]; sizes = [4])
        @test line_transport(`cat`)(["a b", "c"]) == ["a b", "c"] && line_transport(`cat`)(String[]) == String[]
    end

    @testset "costs: the counts of the compiled Lean algorithms stay within the bounds Lean proves" begin
        if isfile(ALGO_EXECUTABLE)
            rows = cost_table(line_transport(Cmd([ALGO_EXECUTABLE])))
            @test length(rows) == 28 && within_bounds(rows)
            cost(a, n) = only(r.comparisons for r in rows if r.algorithm == a && r.n == n)
            # a search that fails looks at everything, and a falling list is the worst case of insertion sort
            @test all(cost("linear_search", n) == n for n in MaithicAlgoRef.COST_SIZES)
            @test all(cost("insertion_sort", n) == div(n * (n - 1), 2) for n in MaithicAlgoRef.COST_SIZES)
            @test cost("binary_search", 512) <= 10 && cost("merge_sort", 512) > 512
            kept = joinpath(@__DIR__, "..", "..", "..", "fixtures", "algolab", "costs.csv")
            isfile(kept) && @test read(kept, String) == cost_csv(rows)
        else
            @test_skip "the Lean executable is not built; `make build-lean` builds it"
        end
    end
end
