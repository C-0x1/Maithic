# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicEmbed

const STATEMENTS = [
    "Nat.add_comm" => "theorem Nat.add_comm (a b : Nat) : a + b = b + a",
    "Nat.mul_comm" => "theorem Nat.mul_comm (a b : Nat) : a * b = b * a",
    "Nat.add_assoc" => "theorem Nat.add_assoc (a b c : Nat) : a + b + c = a + (b + c)",
    "List.length_append" => "theorem List.length_append (xs ys : List α) : (xs ++ ys).length = xs.length + ys.length",
    "List.reverse_reverse" => "theorem List.reverse_reverse (xs : List α) : xs.reverse.reverse = xs",
    "Nat.le_antisymm" => "theorem Nat.le_antisymm (h₁ : a ≤ b) (h₂ : b ≤ a) : a = b",
]

@testset "MaithicEmbed" begin
    @testset "tokens follow the parts of Lean names and symbols" begin
        @test tokenize("Nat.add_comm") == ["nat", "add", "comm"]
        @test tokenize("lengthAppend") == ["length", "append"]
        @test tokenize("∀ a b : Nat, a ≤ b → a < b + 1") == ["forall", "a", "b", "nat", "a", "le", "b", "implies", "a", "lt", "b", "plus", "1"]
        @test tokenize("") == String[]
        @test tokenize("xs.reverse.reverse = xs") == ["xs", "reverse", "reverse", "eq", "xs"]
    end

    @testset "P04-02-01 an embedder returns unit vectors of its dimension" begin
        e = HashedNgramEmbedder(dimension = 128)
        @test e isa Embedder
        @test dimension(e) == 128
        vectors = embed(e, [last(s) for s in STATEMENTS])
        @test length(vectors) == length(STATEMENTS)
        @test all(length(v) == 128 for v in vectors)
        @test all(isapprox(sqrt(sum(abs2, v)), 1.0; atol = 1e-12) for v in vectors)
        @test embed(e, "") == zeros(128)
        @test_throws ArgumentError HashedNgramEmbedder(dimension = 0)
    end

    @testset "P04-02-02 hashed n-grams place related statements close together" begin
        e = HashedNgramEmbedder()
        add_comm = embed(e, "theorem Nat.add_comm (a b : Nat) : a + b = b + a")
        add_comm_again = embed(e, "theorem add_comm' (x y : Nat) : x + y = y + x")
        reverse = embed(e, "theorem List.reverse_reverse (xs : List α) : xs.reverse.reverse = xs")
        @test cosine(add_comm, add_comm_again) > cosine(add_comm, reverse)
        @test cosine(add_comm, add_comm) ≈ 1.0
        @test cosine(add_comm, reverse) < 0.5
        @test cosine(zeros(4), [1.0, 0, 0, 0]) == 0.0
        @test_throws DimensionMismatch cosine([1.0], [1.0, 2.0])
    end

    @testset "P04-02-03 embeddings are identical across runs" begin
        e = HashedNgramEmbedder(dimension = 64)
        text = "theorem Nat.add_comm (a b : Nat) : a + b = b + a"
        @test embed(e, text) == embed(e, text)
        @test embed(HashedNgramEmbedder(dimension = 64), text) == embed(e, text)
        # The hash is defined by the package, so these values hold on every machine.
        @test MaithicEmbed.fnv1a("", UInt64(0)) == 0xcbf29ce484222325
        @test MaithicEmbed.fnv1a("a", UInt64(0)) == 0xaf63dc4c8601ec8c
        @test MaithicEmbed.fnv1a("foobar", UInt64(0)) == 0x85944171f73967e8
        @test embed(HashedNgramEmbedder(dimension = 64, seed = 1), text) != embed(e, text)
    end

    @testset "P04-02-04 queries and documents are preprocessed differently" begin
        e = HashedNgramEmbedder()
        text = "add comm"
        @test embed(e, text, Query) != embed(e, text, Document)
        @test embed(e, text) == embed(e, text, Document)
        @test isapprox(sqrt(sum(abs2, embed(e, text, Query))), 1.0; atol = 1e-12)
    end

    @testset "P04-02-05 vectors survive JSON and binary serialization" begin
        v = embed(HashedNgramEmbedder(dimension = 32), "theorem t : 1 + 1 = 2")
        @test parse_vector_json(vector_json(v)) == v
        @test parse_vector_json("[]") == Float64[]
        @test parse_vector_json(" [1.0, -2.5,3e-2] ") == [1.0, -2.5, 0.03]
        bytes = vector_bytes(v)
        @test length(bytes) == 8 + 4 * 32
        @test bytes[1:4] == UInt8['M', 'V', 'E', 'C']
        @test parse_vector_bytes(bytes) == Float32.(v)
        @test_throws ArgumentError parse_vector_bytes(UInt8[1, 2, 3])
        @test_throws ArgumentError parse_vector_bytes(bytes[1:(end - 1)])
        @test_throws ArgumentError parse_vector_json("1.0")
    end

    @testset "P04-03-01 the flat index finds the exact nearest vectors" begin
        e = HashedNgramEmbedder()
        index = FlatIndex(dimension(e))
        for (id, statement) in STATEMENTS
            add!(index, id, embed(e, statement))
        end
        @test length(index) == length(STATEMENTS)
        query = embed(e, "a + b = b + a", Query)
        hits = search(index, query; k = 3)
        @test hits[1].id == "Nat.add_comm"
        # Exactness: the scores equal the dot products computed one by one.
        expected = sort([(id, sum(embed(e, s) .* query)) for (id, s) in STATEMENTS]; by = p -> (-p[2], p[1]))
        @test [h.id for h in hits] == [first(p) for p in expected[1:3]]
        @test all(isapprox(h.score, p[2]; atol = 1e-12) for (h, p) in zip(hits, expected))
        @test_throws DimensionMismatch add!(index, "bad", [1.0, 2.0])
        @test_throws DimensionMismatch search(index, [1.0])
    end

    @testset "P04-03-02 top-k returns identifiers and scores, best first" begin
        index = FlatIndex(2)
        add!(index, "east", [1.0, 0.0])
        add!(index, "north", [0.0, 1.0])
        add!(index, "northeast", [sqrt(0.5), sqrt(0.5)])
        hits = search(index, [1.0, 0.0]; k = 2)
        @test [h.id for h in hits] == ["east", "northeast"]
        @test hits[1].score ≈ 1.0 && hits[2].score ≈ sqrt(0.5)
        @test length(search(index, [1.0, 0.0]; k = 10)) == 3
        @test issorted([h.score for h in search(index, [0.6, 0.8])]; rev = true)
        @test search(FlatIndex(2), [1.0, 0.0]) == Hit[]
        # Equal scores are ordered by identifier.
        tie = FlatIndex(1)
        add!(tie, "b", [1.0]); add!(tie, "a", [1.0])
        @test [h.id for h in search(tie, [1.0])] == ["a", "b"]
    end

    @testset "P04-03-03 identifiers map to rows stably, and re-adding replaces" begin
        index = FlatIndex(2)
        for i in 1:40
            add!(index, "v$i", [cos(i), sin(i)])
        end
        @test length(index) == 40          # the storage grew past its first capacity
        @test search(index, [cos(7), sin(7)]; k = 1)[1].id == "v7"
        add!(index, "v7", [-cos(7), -sin(7)])
        @test length(index) == 40
        @test search(index, [cos(7), sin(7)]; k = 40)[end].id == "v7"
        @test search(index, [cos(33), sin(33)]; k = 1)[1].id == "v33"
    end
end
