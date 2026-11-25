# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicData
using MaithicCore

const ROOT = normpath(joinpath(@__DIR__, "..", "..", ".."))
const DEMO = joinpath(ROOT, "fixtures", "corpus", "demo")

pair(theorem, state, tactic; mod = "M") = PairRecord("0000000000000000", theorem, mod, state, tactic, 1, 0)
theorem(name; mod = "M", statement = "True", proof = "trivial") = TheoremRecord("0000000000000000", name, mod, "M.lean", 1, statement, proof, 0, 0, true)

@testset "MaithicData" begin
    @testset "P11-05-01 shards are loaded into typed tables" begin
        data = load_dataset(DEMO)
        manifest = read_json(read(joinpath(DEMO, "manifest.json"), String))
        @test data.name == "demo" && data.version == "1"
        @test length(data.theorems) == manifest["records"]["theorem"]
        @test length(data.steps) == manifest["records"]["step"]
        @test length(data.premises) == manifest["records"]["premise"]
        @test length(data.pairs) == manifest["records"]["pair"]
        @test data.theorems isa Vector{TheoremRecord} && data.pairs isa Vector{PairRecord}
        restated = only(t for t in data.theorems if t.name == "Bad_Name")
        @test restated.statement == "∀ (a b : Nat), a + b = b + a" && restated.proof == "Nat.add_comm a b"
        @test restated.complete && restated.steps == 0 && restated.premises == 2
        @test !only(t for t in data.theorems if t.name == "open_one").complete
        steps = [s for s in data.steps if s.theorem == "double_eq"]
        @test [s.index for s in steps] == [0, 1, 2, 3]
        @test steps[2].tactic == "rfl" && steps[2].goals_before == ["case zero\n⊢ double 0 = 2 * 0"] && isempty(steps[2].goals_after)
        @test [p.tactic for p in data.pairs if p.theorem == "double_eq"] == ["rfl", "simp [double]", "omega"]
        @test "Nat.add_comm" in [p.premise for p in data.premises if p.theorem == "Bad_Name"]

        # What does not belong in a shard is refused with the line it stands on.
        good = """{"schema":"maithic.corpus.premise/1","id":"0123456789abcdef","theorem":"t","module":"M","premise":"Nat"}"""
        @test load_records("premise", good * "\n\n") == [PremiseRecord("0123456789abcdef", "t", "M", "Nat")]
        @test only(load_records("premise", good)).premise == "Nat"
        @test_throws DatasetError load_records("premise", "not json")
        @test_throws DatasetError load_records("pair", good)
        @test_throws DatasetError load_records("premise", """{"schema":"maithic.corpus.premise/1","id":"0123456789abcdef"}""")
        @test_throws DatasetError load_records("lemma", good)
        @test_throws DatasetError load_dataset(joinpath(ROOT, "fixtures", "corpus", "nowhere"))

        # A shard that was changed after the manifest was written is not loaded.
        mktempdir() do dir
            cp(DEMO, joinpath(dir, "copy"))
            shard = joinpath(dir, "copy", first(s["file"] for s in manifest["shards"] if s["kind"] == "theorem"))
            chmod(shard, 0o644)
            write(shard, replace(read(shard, String), "double_eq" => "double_xx"))
            err = try load_dataset(joinpath(dir, "copy")); nothing catch e; e end
            @test err isa DatasetError && occursin("does not have the hash", err.message)
        end
    end

    @testset "P11-05-02 identical records are removed" begin
        records = [pair("a", "⊢ 1 = 1", "rfl"), pair("b", "⊢ 1 = 1", "rfl"), pair("a", "⊢ 1 = 1", "simp"), pair("c", "⊢ 1 = 1", "rfl")]
        kept = deduplicate(records)
        # The same state and tactic is the same example, whichever theorem it comes from; the first is kept.
        @test [(r.theorem, r.tactic) for r in kept] == [("a", "rfl"), ("a", "simp")]
        @test duplicates(records) == [[1, 2, 4]]
        @test deduplicate(kept) == kept
        @test isempty(deduplicate(PairRecord[])) && isempty(duplicates(PairRecord[]))
        # Another notion of sameness can be asked for.
        @test length(deduplicate(records; by = r -> r.theorem)) == 3
        theorems = [theorem("x"), theorem("y"), theorem("z"; statement = "1 = 1", proof = "rfl")]
        @test [t.name for t in deduplicate(theorems)] == ["x", "z"]

        data = load_dataset(DEMO)
        unique_pairs = deduplicate(data.pairs)
        @test length(unique_pairs) <= length(data.pairs)
        @test length(unique_pairs) == length(unique((p.state, p.tactic) for p in data.pairs))
        @test length(unique_pairs) + sum(length(group) - 1 for group in duplicates(data.pairs); init = 0) == length(data.pairs)
    end

    @testset "P11-05-03 a split by theorem that a seed determines" begin
        theorems = [theorem("t$i") for i in 1:20]
        split = random_split(theorems; seed = 7)
        counts = split_counts(split)
        @test counts[:test] == 2 && counts[:validation] == 2 && counts[:train] == 16
        @test split.fractions == (0.8, 0.1, 0.1)
        # The same theorems and seed give the same split, in whatever order the records come.
        @test random_split(reverse(theorems); seed = 7).parts == split.parts
        @test random_split(theorems; seed = 8).parts != split.parts
        # Every record goes where its theorem goes.
        @test part_of(split, pair("t3", "s", "rfl")) == split.parts[("M", "t3")]
        @test_throws KeyError part_of(split, pair("unknown", "s", "rfl"))
        # Fractions.
        halves = split_counts(random_split(theorems; seed = 1, validation = 0.0, test = 0.5))
        @test halves[:test] == 10 && halves[:validation] == 0 && halves[:train] == 10
        @test split_counts(random_split(theorems; validation = 0.0, test = 0.0))[:train] == 20
        @test_throws ArgumentError random_split(theorems; validation = 0.6, test = 0.5)
        @test_throws ArgumentError random_split(theorems; test = -0.1)
        # A small dataset still gets a test and a validation part when they are asked for.
        small = split_counts(random_split([theorem("t$i") for i in 1:3]; seed = 0))
        @test small[:test] == 1 && small[:validation] == 1 && small[:train] == 1
        @test isempty(random_split(TheoremRecord[]).parts)
        # Two theorems of one name in different modules are different theorems.
        twins = random_split([theorem("same"; mod = "A"), theorem("same"; mod = "B"), theorem("other"; mod = "A")]; seed = 3)
        @test length(twins.parts) == 3

        doc = split_manifest(split)
        @test doc["schema"] == "maithic.corpus.split/1" && doc["seed"] == 7
        @test doc["theorems"]["train"] == 16 && length(doc["test"]) == 2
        @test issorted([m["name"] for m in doc["train"]])
        @test read_json(write_json(doc)) == doc

        data = load_dataset(DEMO)
        real = random_split(data.theorems; seed = 2026)
        @test sum(values(split_counts(real))) == length(data.theorems)
        @test real.parts == random_split(data.theorems; seed = 2026).parts
    end

    @testset "P11-05-04 no theorem appears in two parts" begin
        data = load_dataset(DEMO)
        split = random_split(data.theorems; seed = 2026)
        in_part(records, part) = [r for r in records if part_of(split, r) == part]
        for records in (data.theorems, data.steps, data.premises, data.pairs)
            parts = [in_part(records, part) for part in (:train, :validation, :test)]
            @test sum(length, parts) == length(records)
            @test check_leakage(parts...)
            @test isempty(leakage(parts...))
        end
        # Steps in one part and pairs in another are compared by the theorem they belong to.
        @test check_leakage(in_part(data.steps, :train), in_part(data.pairs, :test))
        # A split made by record instead of by theorem leaks, and the check says where.
        train = [pair("a", "s1", "rfl"), pair("b", "s2", "rfl")]
        test = [pair("a", "s3", "simp"), pair("c", "s4", "rfl")]
        @test leakage(train, test) == [("M", "a")]
        err = try check_leakage(train, test); nothing catch e; e end
        @test err isa LeakageError && err.theorems == [("M", "a")]
        @test occursin("1 theorem(s) appear in more than one part, the first being M.a", sprint(showerror, err))
        # The same name in another module is another theorem.
        @test check_leakage(train, [pair("a", "s3", "simp"; mod = "Other")])
        @test check_leakage(PairRecord[], PairRecord[])
    end
end
