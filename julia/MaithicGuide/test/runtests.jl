# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicGuide
using MaithicCore
using MaithicData

const G = MaithicGuide
const ROOT = normpath(joinpath(@__DIR__, "..", "..", ".."))
const DEMO = joinpath(ROOT, "fixtures", "corpus", "demo")
const FIXTURE = joinpath(ROOT, "fixtures", "guide", "library")

kinds(text) = [(t.kind, t.text) for t in lex(text)]

# A small problem a model can learn completely: the operator of a goal says
# which tactic closes it, and the names in it say nothing.
function toy_examples(count::Integer; offset::Integer = 0)
    examples = Example[]
    for i in (1:count) .+ offset
        a, b = "a$(i % 7)", "b$(i % 5)"
        shape = i % 4
        if shape == 0
            push!(examples, Example("le$(i)", "$(a) $(b) : Nat\n⊢ $(a) ≤ $(a) + $(b)", "omega", true))
        elseif shape == 1
            push!(examples, Example("app$(i)", "$(a) : List Nat\n⊢ $(a) ++ [] = $(a)", "simp [List.append_nil]", true))
        elseif shape == 2
            push!(examples, Example("and$(i)", "$(a) $(b) : Prop\nh₁ : $(a)\nh₂ : $(b)\n⊢ $(a) ∧ $(b)", "constructor", false))
        else
            push!(examples, Example("sym$(i)", "$(a) $(b) : Nat\nh : $(a) = $(b)\n⊢ $(b) = $(a)", "exact h.symm", true))
        end
    end
    examples
end

# A model whose scores do not depend on the state: the bias of its classifier
# decides, which makes its answers computable by hand.
function fixed_model(templates::Vector{String}, bias::Vector{Float64}; closes = zeros(length(templates)))
    vocabulary = build_vocabulary(["⊢ a = b", "⊢ a = b"]; min_count = 1)
    classes = TemplateVocabulary(templates, ones(Int, length(templates)), Float64.(closes))
    config = ModelConfig(dim = 2, max_length = 4)
    GuideModel(config, vocabulary, classes, Embedding(zeros(2, length(vocabulary)), zeros(2, 4)), nothing, Head(zeros(length(templates), 2), bias))
end

# The gradient of the loss of one example by differences, array by array.
function difference_gradients(model::GuideModel, ids, target; step = 1e-5)
    loss() = cross_entropy(template_scores(model, ids), target)
    map(parameters(model)) do (_, array)
        gradient = zeros(size(array))
        for i in eachindex(array)
            kept = array[i]
            array[i] = kept + step
            above = loss()
            array[i] = kept - step
            below = loss()
            array[i] = kept
            gradient[i] = (above - below) / (2 * step)
        end
        gradient
    end
end

@testset "MaithicGuide" begin
    @testset "P12-01-01 Lean text is split into names, operators, literals and punctuation" begin
        @test kinds("theorem foo (a b : Nat) : a + b ≤ b + a := by omega") ==
              [(:identifier, "theorem"), (:identifier, "foo"), (:punctuation, "("), (:identifier, "a"), (:identifier, "b"), (:operator, ":"),
               (:identifier, "Nat"), (:punctuation, ")"), (:operator, ":"), (:identifier, "a"), (:operator, "+"), (:identifier, "b"),
               (:operator, "≤"), (:identifier, "b"), (:operator, "+"), (:identifier, "a"), (:operator, ":="), (:identifier, "by"),
               (:identifier, "omega")]
        # A dotted name is one name; a hypothesis Lean made inaccessible is one name; so is a name in guillemets.
        @test kinds("Nat.succ_le_of_lt x✝ «a b» a₁ f' g! h?") ==
              [(:identifier, "Nat.succ_le_of_lt"), (:identifier, "x✝"), (:identifier, "«a b»"), (:identifier, "a₁"), (:identifier, "f'"),
               (:identifier, "g!"), (:identifier, "h?")]
        # Projections are not decimals, and decimals are not projections.
        @test kinds("p.1.2 3.14 0x1F 1.succ") ==
              [(:identifier, "p"), (:punctuation, "."), (:number, "1"), (:punctuation, "."), (:number, "2"), (:number, "3.14"), (:number, "0x1F"),
               (:number, "1"), (:punctuation, "."), (:identifier, "succ")]
        @test kinds("\"a \\\" b\" 'a' '\\n'") == [(:string, "\"a \\\" b\""), (:string, "'a'"), (:string, "'\\n'")]
        @test kinds("<;> ∀ λ ℕ ⊢ → => .. ⟨x, y⟩") ==
              [(:operator, "<;>"), (:operator, "∀"), (:identifier, "λ"), (:identifier, "ℕ"), (:operator, "⊢"), (:operator, "→"), (:operator, "=>"),
               (:operator, ".."), (:punctuation, "⟨"), (:identifier, "x"), (:punctuation, ","), (:identifier, "y"), (:punctuation, "⟩")]
        # Comments separate tokens and are not tokens.
        @test kinds("a -- the rest of the line\nb /- a block\n of comment -/ c") == [(:identifier, "a"), (:identifier, "b"), (:identifier, "c")]
        # A text that is cut off inside a string or a comment ends there.
        @test kinds("x \"open") == [(:identifier, "x"), (:string, "\"open")]
        @test kinds("x /- open") == [(:identifier, "x")]
        @test isempty(lex("")) && isempty(lex("  \n\t "))
        @test isliteral(Token(:number, "1")) && isliteral(Token(:string, "\"s\"")) && !isliteral(Token(:identifier, "x"))
        @test Token(:number, "1") == Token(:number, "1") && Token(:number, "1") != Token(:identifier, "1")

        # Nothing of a proof state is lost: its tokens are its text without the space.
        data = load_dataset(DEMO)
        @test all(join(t.text for t in lex(text)) == filter(!isspace, text) for pair in data.pairs for text in (pair.state, pair.tactic))
        @test all(t.kind in (:identifier, :operator, :number, :string, :punctuation) for pair in data.pairs for t in lex(pair.state))
    end

    @testset "P12-01-02 a vocabulary is built with a frequency threshold" begin
        v = build_vocabulary(["a + b", "b + c", "b"]; min_count = 2)
        @test v.tokens == [PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING, "b", "+"]
        @test v.counts == [0, 0, 0, 0, 0, 3, 2]
        @test length(v) == 7
        # The order of the texts does not matter, and equally frequent tokens come in the order of their text.
        @test build_vocabulary(["b", "b + c", "a + b"]; min_count = 2) == v
        @test build_vocabulary(["z y x", "x y z"]; min_count = 1).tokens[6:end] == ["x", "y", "z"]
        @test build_vocabulary(["a + b", "b + c", "b"]; min_count = 1).tokens[6:end] == ["b", "+", "a", "c"]
        # A size limit keeps the most frequent tokens.
        @test build_vocabulary(["a + b", "b + c", "b"]; min_count = 1, max_size = 6).tokens == [PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING, "b"]
        @test length(build_vocabulary(String[])) == 5

        # A token the vocabulary does not hold is counted as the stand-in for its sort.
        @test token_id(v, Token(:identifier, "b")) == 6
        @test token_id(v, Token(:identifier, "unseen")) == 3
        @test token_id(v, Token(:number, "42")) == 4
        @test token_id(v, Token(:string, "\"s\"")) == 5
        @test token_id(v, Token(:operator, "≤")) == 2
        @test token_id(v, Token(:punctuation, "(")) == 2

        @test_throws ArgumentError build_vocabulary(["a"]; min_count = 0)
        @test_throws ArgumentError build_vocabulary(["a"]; max_size = 4)
        @test_throws ArgumentError Vocabulary(["a", "b"], [1, 1])
        @test_throws ArgumentError Vocabulary([PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING, "a", "a"], zeros(Int, 7))
        @test_throws ArgumentError Vocabulary([PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING], [0])

        # On real proof states the frequent tokens are the ones one expects.
        data = load_dataset(DEMO)
        real = build_vocabulary((pair.state for pair in data.pairs); min_count = 2)
        @test "⊢" in real.tokens && ":" in real.tokens && "Nat" in real.tokens
        @test issorted(real.counts[6:end]; rev = true) && all(>=(2), real.counts[6:end])
    end

    @testset "P12-01-03 text is turned into token numbers and back" begin
        text = "h : a ≤ b\n⊢ a ≤ b + 1"
        v = build_vocabulary([text]; min_count = 1)
        ids = encode(v, text)
        @test length(ids) == length(lex(text)) && all(id -> 6 <= id <= length(v), ids)
        @test decode(v, ids) == "h : a ≤ b ⊢ a ≤ b + 1"
        @test encode(v, decode(v, ids)) == ids
        # Tokens the vocabulary does not hold come back as stand-ins, each of its sort.
        @test decode(v, encode(v, "k : a < 22 \"s\"")) == "<id> : a <unk> <num> <str>"
        # A length limit keeps the start of the text.
        @test encode(v, text; max_length = 3) == ids[1:3]
        @test isempty(encode(v, "")) && decode(v, Int[]) == ""
        @test decode(v, [1, ids[1], 1, ids[2], 1]) == "h :"
        @test_throws ArgumentError decode(v, [0])
        @test_throws ArgumentError decode(v, [length(v) + 1])

        # A proof state is read by its first goal, the conclusion before the hypotheses.
        @test main_goal("a : Nat\n⊢ a = a\n\ncase right\n⊢ True") == "a : Nat\n⊢ a = a"
        @test main_goal("⊢ True") == "⊢ True" && main_goal("") == ""
        @test conclusion_first("case succ\nn : Nat\nih : p n\n⊢ p (n + 1)") == "⊢ p (n + 1)\ncase succ\nn : Nat\nih : p n"
        @test conclusion_first("h : a ≤ b\n⊢ a ≤\n    b + 1") == "⊢ a ≤\n    b + 1\nh : a ≤ b"
        @test conclusion_first("⊢ True") == "⊢ True" && conclusion_first("no turnstile") == "no turnstile"
        state = "h : a ≤ b\n⊢ a ≤ b + 1\n\n⊢ b ≤ b"
        @test decode(v, state_ids(v, state)) == "⊢ a ≤ b + 1 h : a ≤ b"
        # When a state is cut, hypotheses are what is lost.
        @test decode(v, state_ids(v, state; max_length = 6)) == "⊢ a ≤ b + 1"
        @test sort(state_ids(v, text)) == sort(ids)
    end

    @testset "P12-02-01 tokens and positions have vectors that are added" begin
        tokens = [1.0 2.0 3.0; 10.0 20.0 30.0]
        positions = [0.1 0.2 0.3 0.4; 0.01 0.02 0.03 0.04]
        e = Embedding(tokens, positions)
        x = embed(e, [3, 1, 3])
        @test size(x) == (2, 3)
        @test x[:, 1] ≈ [3.1, 30.01] && x[:, 2] ≈ [1.2, 10.02] && x[:, 3] ≈ [3.3, 30.03]
        # The same token has another vector at another position.
        @test x[:, 1] != x[:, 3]
        # A sequence longer than the model reads is cut, and an empty one is a single padding token.
        @test embed(e, [2, 2, 2, 2, 2, 2]) == embed(e, [2, 2, 2, 2])
        @test embed(e, Int[]) == reshape([1.1, 10.01], 2, 1)
        @test_throws ArgumentError embed(e, [4])
        @test_throws ArgumentError embed(e, [0])

        # Random weights come from a stream: the same stream, the same embedding.
        a = Embedding(stream(5, "weights"), 9, 4, 6)
        b = Embedding(stream(5, "weights"), 9, 4, 6)
        @test size(a.tokens) == (4, 9) && size(a.positions) == (4, 6)
        @test a.tokens == b.tokens && a.positions == b.positions
        @test a.tokens != Embedding(stream(6, "weights"), 9, 4, 6).tokens
        @test all(abs.(a.tokens) .< 1.0)

        # On the way back a gradient lands on the vector of its token and of its position.
        dtokens, dpositions = zeros(2, 3), zeros(2, 4)
        G.embed_back!(dtokens, dpositions, e, [3, 1, 3], [1.0 2.0 4.0; 1.0 2.0 4.0])
        @test dtokens == [2.0 0.0 5.0; 2.0 0.0 5.0]
        @test dpositions == [1.0 2.0 4.0 0.0; 1.0 2.0 4.0 0.0]
    end

    @testset "P12-02-02 a sequence is pooled to one vector by its mean or its first token" begin
        x = [1.0 2.0 6.0; -1.0 0.0 4.0]
        @test pool(:mean, x) ≈ [3.0, 1.0]
        @test pool(:first, x) == [1.0, -1.0]
        # The vector has the size of the model whatever the length of the sequence.
        @test length(pool(:mean, rand(5, 1))) == length(pool(:mean, rand(5, 40))) == 5
        @test_throws ArgumentError pool(:max, x)
        @test_throws ArgumentError pool(:mean, zeros(2, 0))

        # The way back: a mean shares a gradient among the positions, the first token takes all of it.
        @test G.pool_back(:mean, x, [3.0, 6.0]) ≈ [1.0 1.0 1.0; 2.0 2.0 2.0]
        @test G.pool_back(:first, x, [3.0, 6.0]) == [3.0 0.0 0.0; 6.0 0.0 0.0]

        # The first token alone knows nothing of the rest of the sequence; after a
        # layer of attention it does.
        layer = Attention(stream(2, "weights"), 4)
        one = G.normal_matrix(stream(3, "x"), 4, 5, 1.0)
        other = copy(one)
        other[:, 4] .+= 1.0
        @test pool(:first, one) == pool(:first, other)
        mixed_one, cache = attend(layer, one)
        mixed_other, _ = attend(layer, other)
        @test pool(:first, mixed_one) != pool(:first, mixed_other)
        @test size(mixed_one) == size(one)
        # Every position spreads a weight of one over all positions.
        @test all(sum(cache.weights; dims = 2) .≈ 1.0) && all(cache.weights .> 0.0)
        # With a value matrix of zeros the layer passes its input through.
        silent = Attention(layer.query, layer.key, zeros(4, 4))
        @test attend(silent, one)[1] == one
    end

    @testset "P12-02-03 a classifier scores every tactic template" begin
        vocabulary = build_vocabulary(["⊢ a ≤ b + 1", "h : a = b", "⊢ a ∧ b"]; min_count = 1)
        templates = TemplateVocabulary([OTHER, "omega", "simp [_]", "exact _"], [1, 4, 3, 2], [0.5, 1.0, 0.5, 1.0])
        ids = encode(vocabulary, "h : a ≤ b ⊢ b = a ∧ a ≤ a")
        for mixer in (:none, :attention), pooling in (:mean, :first)
            model = GuideModel(ModelConfig(dim = 4, max_length = 8, pooling = pooling, mixer = mixer), vocabulary, templates; seed = 11)
            scores = template_scores(model, ids)
            @test length(scores) == 4 && all(isfinite, scores)
            p = template_probabilities(model, "h : a ≤ b\n⊢ b = a")
            @test length(p) == 4 && sum(p) ≈ 1.0 && all(>(0.0), p)
            # An empty state is read as padding, not refused.
            @test sum(template_probabilities(model, "")) ≈ 1.0

            names = [name for (name, _) in parameters(model)]
            @test names == (mixer === :attention ? ["tokens", "positions", "query", "key", "value", "weights", "bias"] : ["tokens", "positions", "weights", "bias"])
            @test size(model.head.weights) == (4, 4) && size(model.embedding.tokens) == (4, length(vocabulary)) && size(model.embedding.positions) == (4, 8)

            # The gradients the model computes are the gradients of its loss.
            for target in (1, 3)
                computed = G.zero_gradients(model)
                loss = G.accumulate!(computed, model, ids, target)
                @test loss ≈ cross_entropy(template_scores(model, ids), target)
                expected = difference_gradients(model, ids, target)
                for (k, (name, _)) in enumerate(parameters(model))
                    @test size(computed[k]) == size(expected[k])
                    @test isapprox(computed[k], expected[k]; atol = 1e-7, rtol = 1e-5)
                end
                # Gradients are added to what is there: twice the example, twice the gradient.
                G.accumulate!(computed, model, ids, target)
                @test all(isapprox(computed[k], 2 .* expected[k]; atol = 2e-7, rtol = 1e-5) for k in eachindex(computed))
            end
            # A sequence longer than the model reads has gradients too, for the part that is read.
            long = repeat(ids, 3)
            computed = G.zero_gradients(model)
            G.accumulate!(computed, model, long, 2)
            expected = difference_gradients(model, long, 2)
            @test all(isapprox(computed[k], expected[k]; atol = 1e-7, rtol = 1e-5) for k in eachindex(computed))
        end

        # A seed gives a model; another seed, another model.
        config = ModelConfig(dim = 4, max_length = 8, mixer = :attention)
        a, b = GuideModel(config, vocabulary, templates; seed = 3), GuideModel(config, vocabulary, templates; seed = 3)
        @test all(x == y for ((_, x), (_, y)) in zip(parameters(a), parameters(b)))
        @test parameters(a)[1][2] != parameters(GuideModel(config, vocabulary, templates; seed = 4))[1][2]
        @test_throws ArgumentError GuideModel(ModelConfig(dim = 0), vocabulary, templates)
        @test_throws ArgumentError GuideModel(ModelConfig(max_length = 0), vocabulary, templates)
        @test_throws ArgumentError GuideModel(ModelConfig(pooling = :max), vocabulary, templates)
        @test_throws ArgumentError GuideModel(ModelConfig(mixer = :recurrent), vocabulary, templates)
    end

    @testset "P12-02-03 a model is written and read back weight for weight" begin
        vocabulary = build_vocabulary(["⊢ a ≤ b + 1", "h : a = b"]; min_count = 1)
        templates = TemplateVocabulary([OTHER, "omega", "simp [_]"], [1, 4, 3], [0.5, 1.0, 1 / 3])
        for mixer in (:none, :attention)
            model = GuideModel(ModelConfig(dim = 3, max_length = 5, pooling = :first, mixer = mixer), vocabulary, templates; seed = 9)
            doc = model_document(model)
            @test doc["schema"] == MODEL_SCHEMA == "maithic.guide.model/1"
            @test doc["config"]["mixer"] == String(mixer) && doc["config"]["pooling"] == "first" && doc["config"]["dim"] == 3
            @test haskey(doc["weights"], "query") == (mixer === :attention)
            back = model_of(read_json(write_json(doc)))
            @test back.config == model.config && back.vocabulary == model.vocabulary && back.templates == model.templates
            @test all(x == y for ((_, x), (_, y)) in zip(parameters(model), parameters(back)))
            @test template_probabilities(back, "⊢ a ≤ b") == template_probabilities(model, "⊢ a ≤ b")
            mktempdir() do dir
                path = joinpath(dir, "model.json")
                save_model(path, model)
                @test endswith(read(path, String), "\n")
                @test template_probabilities(load_model(path), "h : a = b") == template_probabilities(model, "h : a = b")
                @test_throws ArgumentError load_model(joinpath(dir, "absent.json"))
                write(joinpath(dir, "broken.json"), "{")
                @test_throws ArgumentError load_model(joinpath(dir, "broken.json"))
            end
        end
        model = GuideModel(ModelConfig(dim = 3, max_length = 5), vocabulary, templates; seed = 9)
        # A document of another kind, an incomplete one and one whose shapes disagree are refused.
        @test_throws ArgumentError model_of(Object("schema" => "maithic.guide.run/1"))
        @test_throws ArgumentError model_of(Object("schema" => MODEL_SCHEMA))
        short = read_json(write_json(model_document(model)))
        pop!(short["weights"]["head"])
        @test_throws ArgumentError model_of(short)
        ragged = read_json(write_json(model_document(model)))
        pop!(ragged["weights"]["tokens"][2])
        @test_throws ArgumentError model_of(ragged)
        wrong = read_json(write_json(model_document(model)))
        wrong["config"]["mixer"] = "attention"
        @test_throws ArgumentError model_of(wrong)
    end

    @testset "P12-03-01 tactics become templates with slots for their arguments" begin
        @test template("omega") == "omega"
        @test template("simp [double]") == "simp [_]"
        @test template("simp [List.length_append, Nat.add_comm]") == "simp [_]"
        @test template("simp only [foo, bar] at h ⊢") == "simp only [_] at _"
        @test template("simp [f (g [1])] at h") == "simp [_] at _"
        @test template("simp at *") == "simp at _"
        @test template("rw [foo] at h1 h2") == "rw [_] at _"
        @test template("exact h.trans (le_of_lt hk)") == "exact _"
        @test template("exact ⟨1, by simp⟩") == "exact _"
        @test template("apply Nat.le_antisymm") == "apply _"
        @test template("intro n hn") == "intro _"
        @test template("rcases h with ⟨a, b⟩") == "rcases _ with _"
        @test template("induction xs generalizing ys with") == "induction _ generalizing _ with"
        @test template("simpa using h") == "simpa using _"
        @test template("have h : a = b := by omega") == "have _"
        @test template("  omega  ") == "omega" && template("") == ""
        # The lemma is gone and the move is left: two calls of one tactic are one class.
        @test template("simp [double]") == template("simp [triple, Nat.mul_comm]") != template("simp")

        examples = [Example("t", "⊢ p", tactic, closes) for (tactic, closes) in
                    [("omega", true), ("simp [a]", true), ("omega", true), ("simp [b]", false), ("omega", false), ("intro x", false),
                     ("rfl", true), ("exact h", true), ("intro y", false)]]
        v = build_templates(examples; min_count = 2)
        # The most frequent first, equally frequent ones by their text, and the rest under one class.
        @test v.templates == [OTHER, "omega", "intro _", "simp [_]"]
        @test v.counts == [2, 3, 2, 2]
        @test v.closes ≈ [1.0, 2 / 3, 0.0, 0.5]
        @test length(v) == 4
        @test class_of(v, "omega") == 2 && class_of(v, "simp [anything, at_all]") == 4 && class_of(v, "intro a b c") == 3
        @test class_of(v, "exact foo") == 1 && class_of(v, "decide") == 1
        @test build_templates(examples; min_count = 1).templates == [OTHER, "omega", "intro _", "simp [_]", "exact _", "rfl"]
        @test build_templates(examples; min_count = 1).counts[1] == 0
        @test build_templates(Example[]).templates == [OTHER]
        @test_throws ArgumentError TemplateVocabulary(["omega"], [1], [1.0])
        @test_throws ArgumentError TemplateVocabulary([OTHER, "omega"], [1], [1.0, 1.0])

        # On recorded proofs the templates are few and cover most steps.
        loaded = load_examples(DEMO; seed = 0)
        every = vcat(loaded.train, loaded.validation, loaded.test)
        real = build_templates(every)
        @test "exact _" in real.templates && "omega" in real.templates && "simp [_]" in real.templates
        @test sum(real.counts) == length(every) && real.counts[1] < length(every) ÷ 2
        @test all(0.0 .<= real.closes .<= 1.0)
    end

    @testset "P12-03-02 state and tactic pairs are loaded and split by theorem" begin
        data = load_dataset(DEMO)
        loaded = load_examples(DEMO; seed = 3)
        every = vcat(loaded.train, loaded.validation, loaded.test)
        @test loaded.dataset == "demo" && loaded.version == "1"
        @test loaded.pairs == length(every) == length(deduplicate(data.pairs))
        @test loaded.theorems == length(unique(e.theorem for e in every))
        @test !isempty(loaded.train) && !isempty(loaded.validation) && !isempty(loaded.test)
        # Every step of a proof is in one part.
        names(part) = Set(e.theorem for e in part)
        @test isempty(intersect(names(loaded.train), names(loaded.validation)))
        @test isempty(intersect(names(loaded.train), names(loaded.test)))
        @test isempty(intersect(names(loaded.validation), names(loaded.test)))
        # An example holds the goal the tactic acts on, and whether the tactic closed a goal.
        step = only(e for e in every if endswith(e.theorem, "double_eq") && e.tactic == "rfl")
        @test step.state == "case zero\n⊢ double 0 = 2 * 0" && step.closes
        @test all(!occursin("\n\n", e.state) for e in every)

        # The seed decides the split, and the same seed gives it again.
        again = load_examples(data; seed = 3)
        @test [e.state for e in again.test] == [e.state for e in loaded.test] && [e.tactic for e in again.train] == [e.tactic for e in loaded.train]
        @test any(names(load_examples(data; seed = s).test) != names(loaded.test) for s in 4:8)
        # The fractions are honoured by theorems; none asked for means none.
        whole = load_examples(data; seed = 3, validation = 0.0, test = 0.0)
        @test length(whole.train) == loaded.pairs && isempty(whole.validation) && isempty(whole.test)
        @test_throws ArgumentError load_examples(data; validation = 0.6, test = 0.5)
        @test_throws DatasetError load_examples(joinpath(ROOT, "fixtures", "corpus", "nowhere"))

        # Pairs that are the same example are taken once; pairs without their theorem are refused.
        record(name) = TheoremRecord("0000000000000000", name, "M", "M.lean", 1, "True", "by trivial", 1, 0, true)
        pair(theorem, state, tactic, before, after) = PairRecord("0000000000000000", theorem, "M", state, tactic, before, after)
        small = Dataset("small", "1", [record("a"), record("b"), record("c"), record("unused")], StepRecord[], PremiseRecord[],
                        [pair("a", "⊢ True", "trivial", 1, 0), pair("b", "⊢ True", "trivial", 1, 0), pair("b", "⊢ 1 = 1\n\n⊢ 2 = 2", "rfl", 2, 1),
                         pair("c", "⊢ p → p", "intro h", 1, 1)])
        loaded = load_examples(small; seed = 0)
        every = vcat(loaded.train, loaded.validation, loaded.test)
        @test loaded.pairs == 3 && loaded.theorems == 3
        @test sort([(e.state, e.tactic, e.closes) for e in every]) == [("⊢ 1 = 1", "rfl", true), ("⊢ True", "trivial", true), ("⊢ p → p", "intro h", false)]
        orphan = Dataset("orphan", "1", [record("a")], StepRecord[], PremiseRecord[], [pair("z", "⊢ True", "trivial", 1, 0)])
        @test_throws ArgumentError load_examples(orphan)
    end

    @testset "P12-03-03 the loss is the cross-entropy over templates" begin
        @test cross_entropy([0.0, 0.0, 0.0], 2) ≈ log(3)
        @test cross_entropy([0.0, 50.0, 0.0], 2) < 1e-12
        @test cross_entropy([50.0, 0.0, 0.0], 2) ≈ 50.0 atol = 1e-9
        @test cross_entropy([1.0, 2.0, 3.0], 3) ≈ -log(exp(3.0) / (exp(1.0) + exp(2.0) + exp(3.0)))
        # Scores of any size give finite numbers.
        @test isfinite(cross_entropy([1e4, -1e4, 0.0], 2)) && cross_entropy([1e4, -1e4, 0.0], 1) == 0.0
        p = softmax([1.0, 2.0, 3.0])
        @test sum(p) ≈ 1.0 && issorted(p) && p ≈ exp.(log_softmax([1.0, 2.0, 3.0]))
        @test softmax([7.0, 7.0]) ≈ [0.5, 0.5]
        # Adding the same number to every score changes nothing.
        @test softmax([1.0, 2.0, 3.0] .+ 100.0) ≈ p

        # The gradient is the probabilities less one at the target, and it is the gradient.
        scores = [0.3, -1.2, 2.0, 0.0]
        gradient = cross_entropy_gradient(scores, 3)
        @test gradient ≈ softmax(scores) .- [0.0, 0.0, 1.0, 0.0]
        @test abs(sum(gradient)) < 1e-12
        for i in eachindex(scores)
            above, below = copy(scores), copy(scores)
            above[i] += 1e-6
            below[i] -= 1e-6
            @test gradient[i] ≈ (cross_entropy(above, 3) - cross_entropy(below, 3)) / 2e-6 atol = 1e-8
        end
        @test_throws ArgumentError cross_entropy(scores, 0)
        @test_throws ArgumentError cross_entropy(scores, 5)
        @test_throws ArgumentError cross_entropy_gradient(scores, 5)
    end

    @testset "P12-03-04 the classifier is trained with an evaluation after every epoch" begin
        examples, held_out = toy_examples(80), toy_examples(40; offset = 1000)
        config = TrainConfig(model = ModelConfig(dim = 8, max_length = 24), epochs = 12, batch = 8, seed = 1)
        result = train(examples, held_out; config = config)
        @test length(result.log) == 12 && [e.epoch for e in result.log] == collect(1:12)
        # The loss falls, and what can be learned is learned.
        @test result.log[end].train_loss < 0.5 * result.log[1].train_loss
        @test result.log[1].train_loss < log(5) + 0.5
        @test result.model.templates.templates == [OTHER, "constructor", "exact _", "omega", "simp [_]"]
        @test evaluate(result.model, held_out).top1 == 1.0
        @test first(top_templates(result.model, "x y : Nat\n⊢ x ≤ x + y", 1))[1] == "omega"
        @test first(top_templates(result.model, "l : List Nat\n⊢ l ++ [] = l", 1))[1] == "simp [_]"
        # The model returned is that of the epoch that was best on the validation part.
        losses = [e.validation.loss for e in result.log]
        @test result.best_epoch == argmin(losses)
        @test evaluate(result.model, held_out).loss == minimum(losses)
        @test all(e.validation.examples == 40 && e.validation.k == 5 for e in result.log)
        # The vocabulary and the templates come from the training examples alone.
        @test !("a1000" in result.model.vocabulary.tokens) && "≤" in result.model.vocabulary.tokens

        # Without a validation part the model of the last epoch is returned.
        alone = train(examples; config = TrainConfig(model = ModelConfig(dim = 4, max_length = 24), epochs = 3, seed = 1))
        @test alone.best_epoch == 3 && all(e.validation.examples == 0 && isnan(e.validation.loss) for e in alone.log)
        # Every shape of model trains.
        for mixer in (:none, :attention), pooling in (:mean, :first)
            shaped = train(examples, held_out; config = TrainConfig(model = ModelConfig(dim = 8, max_length = 24, pooling = pooling, mixer = mixer),
                                                                    epochs = 4, batch = 8, seed = 2))
            @test shaped.log[end].train_loss < shaped.log[1].train_loss
            @test all(isfinite(e.train_loss) && isfinite(e.validation.loss) for e in shaped.log)
        end
        @test_throws ArgumentError train(Example[])
        @test_throws ArgumentError train(examples; config = TrainConfig(epochs = 0))
        @test_throws ArgumentError train(examples; config = TrainConfig(batch = 0))
        @test_throws ArgumentError train(examples; config = TrainConfig(optimizer = Adam(rate = 0.0)))
        @test_throws ArgumentError train(examples; config = TrainConfig(model = ModelConfig(pooling = :max)))
    end

    @testset "P12-03-04 a run is recorded with its configuration, its data and every epoch" begin
        loaded = load_examples(DEMO; seed = 3)
        config = TrainConfig(model = ModelConfig(dim = 8), epochs = 5, batch = 8, seed = 3)
        result = train(loaded.train, loaded.validation; config = config)
        record = run_document("demo-mean-8", config, loaded, result)
        @test record["schema"] == RUN_SCHEMA == "maithic.guide.run/1"
        @test record["name"] == "demo-mean-8" && record["seed"] == 3
        @test record["config"]["dim"] == 8 && record["config"]["pooling"] == "mean" && record["config"]["mixer"] == "none"
        @test record["config"]["epochs"] == 5 && record["config"]["rate"] == 0.01 && record["config"]["decoupled"] == true
        @test record["dataset"]["name"] == "demo" && record["dataset"]["pairs"] == loaded.pairs
        @test record["dataset"]["train"] + record["dataset"]["validation"] + record["dataset"]["test"] == loaded.pairs
        @test record["vocabulary"] == length(result.model.vocabulary) && record["classes"] == length(result.model.templates)
        @test record["parameters"] == sum(length(array) for (_, array) in parameters(result.model))
        @test length(record["epochs"]) == 5 && [e["epoch"] for e in record["epochs"]] == collect(1:5)
        @test all(e["train_loss"] isa Float64 && e["validation_loss"] isa Float64 && 0.0 <= e["validation_top1"] <= e["validation_topk"] <= 1.0
                  for e in record["epochs"])
        @test record["best_epoch"] == result.best_epoch
        @test record["test"]["examples"] == length(loaded.test) == record["baseline"]["examples"]
        @test record["test"]["k"] == 5 && 0.0 <= record["test"]["top1"] <= record["test"]["topk"] <= 1.0
        # The record says nothing of when and where: the same run, the same record.
        text = write_json(record; pretty = true)
        @test text == write_json(run_document("demo-mean-8", config, loaded, train(loaded.train, loaded.validation; config = config)); pretty = true)
        @test !occursin(ROOT, text) && !occursin("time", text)
        @test read_json(text) == record
    end

    @testset "P12-03-05 accuracy is counted at the top and among the top k" begin
        # The model prefers omega, then simp [_], then exact _, whatever the state.
        model = fixed_model([OTHER, "omega", "simp [_]", "exact _"], [0.0, 3.0, 2.0, 1.0])
        examples = [Example("t", "⊢ a = b", tactic, true) for tactic in ["omega", "omega", "simp [x]", "exact h", "decide", "simp [y, z]", "exact rfl", "omega"]]
        @test topk_accuracy(model, examples, 1) == 3 / 8
        @test topk_accuracy(model, examples, 2) == 5 / 8
        @test topk_accuracy(model, examples, 3) == 7 / 8
        @test topk_accuracy(model, examples, 4) == 1.0 == topk_accuracy(model, examples, 40)
        report = evaluate(model, examples; k = 2)
        @test report.examples == 8 && report.top1 == 3 / 8 && report.topk == 5 / 8 && report.k == 2
        p = softmax([0.0, 3.0, 2.0, 1.0])
        @test report.loss ≈ -(3 * log(p[2]) + 2 * log(p[3]) + 2 * log(p[4]) + log(p[1])) / 8
        @test [name for (name, _) in top_templates(model, "⊢ a = b", 3)] == ["omega", "simp [_]", "exact _"]
        @test [share for (_, share) in top_templates(model, "⊢ a = b", 4)] ≈ p[[2, 3, 4, 1]]
        @test length(top_templates(model, "⊢ a = b", 40)) == 4
        # Equal scores are ranked in the order of the classes, so the count does not depend on a sort.
        level = fixed_model([OTHER, "omega", "simp [_]", "exact _"], [0.0, 0.0, 0.0, 0.0])
        @test topk_accuracy(level, examples, 1) == 1 / 8 && topk_accuracy(level, examples, 2) == 4 / 8
        # Nothing to count gives no number, not a zero.
        empty = evaluate(model, Example[])
        @test empty.examples == 0 && isnan(empty.top1) && isnan(empty.topk) && isnan(empty.loss)
        @test_throws ArgumentError evaluate(model, examples; k = 0)

        # The baseline answers with the most frequent templates of training and knows no state.
        classes = TemplateVocabulary([OTHER, "omega", "simp [_]", "exact _"], [1, 2, 6, 3], zeros(4))
        base = baseline(classes, examples; k = 2)
        @test base.top1 == 2 / 8 && base.topk == 4 / 8 && base.examples == 8
        frequencies = [2, 3, 7, 4] ./ 16
        @test base.loss ≈ -(3 * log(frequencies[2]) + 2 * log(frequencies[3]) + 2 * log(frequencies[4]) + log(frequencies[1])) / 8
        @test baseline(classes, Example[]).examples == 0

        # On a held-out part of a learnable problem a trained model beats the baseline.
        trained = train(toy_examples(80); config = TrainConfig(model = ModelConfig(dim = 8, max_length = 24), epochs = 10, batch = 8, seed = 1)).model
        held_out = toy_examples(40; offset = 500)
        @test topk_accuracy(trained, held_out, 1) > baseline(trained.templates, held_out).top1 + 0.5
        @test topk_accuracy(trained, held_out, 1) <= topk_accuracy(trained, held_out, 2) <= topk_accuracy(trained, held_out, 5) == 1.0
    end

    @testset "P12-03-06 candidates get probabilities that add up to one" begin
        model = fixed_model([OTHER, "omega", "simp [_]", "exact _"], [0.0, 3.0, 2.0, 1.0]; closes = [0.5, 1.0, 0.25, 1.0])
        p = softmax([0.0, 3.0, 2.0, 1.0])
        state = "h : a = b\n⊢ a = b"
        # One candidate for each known template: the probabilities of the templates, scaled to the candidates.
        priors = tactic_priors(model, state, ["exact h", "omega", "simp [h]"])
        @test sum(priors) ≈ 1.0 && all(>(0.0), priors)
        @test priors ≈ [p[4], p[2], p[3]] ./ (p[4] + p[2] + p[3])
        @test argmax(priors) == 2
        # Candidates of one template share its probability equally.
        shared = tactic_priors(model, state, ["simp [h]", "simp [h.symm]", "omega"])
        @test shared[1] == shared[2] && shared[1] ≈ (p[3] / 2) / (p[3] + p[2]) && sum(shared) ≈ 1.0
        # Tactics the model has no template for share the class of all other tactics.
        unknown = tactic_priors(model, state, ["decide", "rfl", "omega"])
        @test unknown[1] == unknown[2] && unknown ≈ [p[1] / 2, p[1] / 2, p[2]] ./ (p[1] + p[2])
        @test tactic_priors(model, state, ["omega"]) == [1.0]
        @test tactic_priors(model, state, String[]) == Float64[]
        # Only the first goal of a state is read.
        @test tactic_priors(model, state * "\n\n⊢ False", ["omega", "exact h"]) == tactic_priors(model, state, ["omega", "exact h"])

        # The value of a state is the chance that the next tactic closes a goal.
        @test state_value(model, state) ≈ p[1] * 0.5 + p[2] * 1.0 + p[3] * 0.25 + p[4] * 1.0
        @test 0.0 <= state_value(model, "") <= 1.0
        @test state_value(fixed_model([OTHER, "omega"], [0.0, 0.0]; closes = [0.0, 0.0]), state) == 0.0
        @test state_value(fixed_model([OTHER, "omega"], [0.0, 0.0]; closes = [1.0, 1.0]), state) == 1.0

        # Premises are ranked by the parts of their names that occur in the state.
        scores = premise_scores(model, "xs ys : List Nat\n⊢ (xs ++ ys).length = xs.length + ys.length", ["List.length_append", "Int.mul_comm", "List.length", "length"])
        @test length(scores) == 4 && all(0.0 .<= scores .<= 1.0)
        @test scores[2] == 0.0 && scores[1] > 0.0 && scores[3] ≈ 1.0 && scores[3] > scores[1]
        @test premise_scores(model, "⊢ True", String[]) == Float64[]
        @test premise_scores(model, "⊢ True", ["", "Nat.succ"]) == [0.0, 0.0]
        @test G.name_parts("List.lengthAppend_nil'") == ["list", "length", "append", "nil'"]

        # A trained model puts the tactic that fits the goal first among the candidates.
        trained = train(toy_examples(80); config = TrainConfig(model = ModelConfig(dim = 8, max_length = 24), epochs = 10, batch = 8, seed = 1)).model
        candidates = ["constructor", "omega", "simp [List.append_nil]", "exact h.symm", "decide"]
        @test argmax(tactic_priors(trained, "m n : Nat\n⊢ m ≤ m + n", candidates)) == 2
        @test argmax(tactic_priors(trained, "p q : Prop\nh₁ : p\nh₂ : q\n⊢ p ∧ q", candidates)) == 1
        @test sum(tactic_priors(trained, "⊢ something else entirely", candidates)) ≈ 1.0
    end

    @testset "P12-03-06 the service answers one request on a line" begin
        model = fixed_model([OTHER, "omega", "simp [_]", "exact _"], [0.0, 3.0, 2.0, 1.0]; closes = [0.5, 1.0, 0.25, 1.0])
        state = "h : a = b\n⊢ a = b"
        hello = answer(model, Object("id" => 1, "method" => "handshake"))
        @test hello["id"] == 1 && hello["service"] == SERVICE == "maithic.guide.service/1"
        @test hello["model"]["classes"] == 4 && hello["model"]["dim"] == 2 && hello["model"]["pooling"] == "mean"
        ranked = answer(model, Object("id" => "r7", "method" => "tactic", "state" => state, "candidates" => Any["omega", "exact h"]))
        @test ranked["id"] == "r7" && ranked["priors"] == tactic_priors(model, state, ["omega", "exact h"]) && ranked["templates"] == ["omega", "exact _"]
        @test answer(model, Object("id" => 2, "method" => "value", "state" => state))["value"] == state_value(model, state)
        @test answer(model, Object("id" => 3, "method" => "premise", "state" => state, "candidates" => Any["Eq.symm", "a"]))["scores"] ==
              premise_scores(model, state, ["Eq.symm", "a"])
        @test answer(model, Object("id" => 4, "method" => "shutdown"))["ok"] == true
        # What cannot be answered is answered with the reason, under the same id.
        @test occursin("no method", answer(model, Object("id" => 5, "method" => "rank"))["error"])
        @test occursin("names no method", answer(model, Object("id" => 6))["error"])
        @test occursin("\"state\"", answer(model, Object("id" => 7, "method" => "value"))["error"])
        @test occursin("\"candidates\"", answer(model, Object("id" => 8, "method" => "tactic", "state" => state, "candidates" => Any[1, 2]))["error"])
        @test answer(model, Object("id" => 9, "method" => "value", "state" => 3))["id"] == 9
        @test occursin("JSON object", answer(model, Any[1, 2])["error"])

        requests = join([write_json(Object("id" => 1, "method" => "handshake")), "", "not json",
                         write_json(Object("id" => 2, "method" => "tactic", "state" => state, "candidates" => Any["omega", "simp [h]", "decide"])),
                         write_json(Object("id" => 3, "method" => "nonsense")), write_json(Object("id" => 4, "method" => "shutdown")),
                         write_json(Object("id" => 5, "method" => "value", "state" => state))], "\n")
        output = IOBuffer()
        serve(model; input = IOBuffer(requests), output = output)
        answers = [read_json(line) for line in split(String(take!(output)), '\n'; keepempty = false)]
        # One answer for each request up to the shutdown; the empty line is no request.
        @test length(answers) == 5
        @test [get(a, "id", 0) for a in answers] == [1, nothing, 2, 3, 4]
        @test occursin("not JSON", answers[2]["error"]) && haskey(answers[4], "error") && answers[5]["ok"] == true
        @test sum(answers[3]["priors"]) ≈ 1.0 && answers[3]["templates"] == ["omega", "simp [_]", "decide"]
    end

    @testset "P12-04-01 Adam, with weight decay apart from the gradient or in it" begin
        # The first step of Adam moves every weight by the rate, against its gradient.
        weights, gradient = [1.0 -2.0; 0.5 4.0], [0.3 -0.1; 2.0 -7.0]
        arrays = Array{Float64}[copy(weights)]
        state = AdamState(arrays)
        step!(Adam(rate = 0.01), state, arrays, [gradient])
        @test arrays[1] ≈ weights .- 0.01 .* sign.(gradient) atol = 1e-8
        @test state.steps == 1

        # It finds the minimum of a bowl.
        target = [3.0, -1.0, 0.5]
        arrays = Array{Float64}[zeros(3)]
        state = AdamState(arrays)
        optimizer = Adam(rate = 0.05)
        for _ in 1:2000
            step!(optimizer, state, arrays, [2.0 .* (arrays[1] .- target)])
        end
        @test arrays[1] ≈ target atol = 1e-3
        @test state.steps == 2000

        # AdamW: the decay shrinks the weights directly, and a gradient of zero moves nothing else.
        arrays = Array{Float64}[copy(weights)]
        step!(Adam(rate = 0.1, weight_decay = 0.5), AdamState(arrays), arrays, [zeros(2, 2)])
        @test arrays[1] ≈ weights .* (1.0 - 0.1 * 0.5)
        # Adam with L2: the decay is part of the gradient, so the first step is again a step of the rate.
        arrays = Array{Float64}[copy(weights)]
        step!(Adam(rate = 0.1, weight_decay = 0.5, decoupled = false), AdamState(arrays), arrays, [zeros(2, 2)])
        @test arrays[1] ≈ weights .- 0.1 .* sign.(weights) atol = 1e-6
        # A bias is not decayed, in either form.
        for decoupled in (true, false)
            arrays = Array{Float64}[[1.0, -2.0]]
            step!(Adam(rate = 0.1, weight_decay = 0.5, decoupled = decoupled), AdamState(arrays), arrays, [zeros(2)])
            @test arrays[1] == [1.0, -2.0]
        end
        # Without decay the two forms are one optimizer.
        a, b = Array{Float64}[copy(weights)], Array{Float64}[copy(weights)]
        sa, sb = AdamState(a), AdamState(b)
        for k in 1:5
            step!(Adam(decoupled = true), sa, a, [gradient ./ k])
            step!(Adam(decoupled = false), sb, b, [gradient ./ k])
        end
        @test a[1] == b[1]
        # With decay a model trained longer has smaller weights than one trained without.
        examples = toy_examples(40)
        norm(model) = sum(abs2, model.head.weights)
        plain = train(examples; config = TrainConfig(model = ModelConfig(dim = 4, max_length = 24), optimizer = Adam(weight_decay = 0.0), epochs = 6, seed = 1)).model
        decayed = train(examples; config = TrainConfig(model = ModelConfig(dim = 4, max_length = 24), optimizer = Adam(weight_decay = 0.5), epochs = 6, seed = 1)).model
        @test norm(decayed) < norm(plain)

        @test_throws ArgumentError G.check(Adam(rate = 0.0))
        @test_throws ArgumentError G.check(Adam(beta1 = 1.0))
        @test_throws ArgumentError G.check(Adam(epsilon = 0.0))
        @test_throws ArgumentError G.check(Adam(weight_decay = -0.1))
        arrays = Array{Float64}[zeros(2)]
        @test_throws ArgumentError step!(Adam(), AdamState(arrays), arrays, [zeros(3)])
        @test_throws ArgumentError step!(Adam(), AdamState(arrays), arrays, [zeros(2), zeros(2)])
    end

    @testset "P12-04-02 one seed decides everything that is random" begin
        # A stream is fixed by its seed and its purpose, on every machine.
        @test next_u64!(stream(0, "weights")) == 1863327315982597904
        @test next_u64!(stream(7, "order")) == 2311471102779129411
        @test G.name_hash("") == 0xcbf29ce484222325 && G.name_hash("weights") == 12774824757867200798
        draw(seed, purpose) = (rng = stream(seed, purpose); [next_u64!(rng) for _ in 1:4])
        @test draw(1, "weights") == draw(1, "weights")
        @test draw(1, "weights") != draw(2, "weights") && draw(1, "weights") != draw(1, "order")
        # Drawing for one purpose does not move the stream of another.
        weights, order = stream(4, "weights"), stream(4, "order")
        foreach(_ -> next_u64!(weights), 1:100)
        @test [next_u64!(order) for _ in 1:4] == draw(4, "order")

        # The normal numbers have the mean and the deviation they should.
        rng = stream(9, "normal")
        sample = [normal!(rng) for _ in 1:20000]
        mean = sum(sample) / length(sample)
        @test abs(mean) < 0.03 && abs(sqrt(sum(abs2, sample .- mean) / length(sample)) - 1.0) < 0.03
        @test count(x -> abs(x) > 3.0, sample) < 120

        # The same seed gives the same run, weight for weight and epoch for epoch.
        examples, held_out = toy_examples(48), toy_examples(16; offset = 300)
        run(seed; dim = 6, mixer = :attention) =
            train(examples, held_out; config = TrainConfig(model = ModelConfig(dim = dim, max_length = 24, mixer = mixer), epochs = 3, batch = 8, seed = seed))
        first_run, second_run = run(5), run(5)
        @test write_json(model_document(first_run.model)) == write_json(model_document(second_run.model))
        @test [e.train_loss for e in first_run.log] == [e.train_loss for e in second_run.log]
        @test first_run.best_epoch == second_run.best_epoch
        other = run(6)
        @test write_json(model_document(other.model)) != write_json(model_document(first_run.model))
        @test [e.train_loss for e in other.log] != [e.train_loss for e in first_run.log]
        # The seed decides the split of a dataset as well.
        data = load_dataset(DEMO)
        @test [e.theorem for e in load_examples(data; seed = 5).test] == [e.theorem for e in load_examples(data; seed = 5).test]
    end

    @testset "P12-03-06 the recorded model answers" begin
        model = load_model(joinpath(FIXTURE, "model.json"))
        record = read_json(read(joinpath(FIXTURE, "run.json"), String))
        @test record["schema"] == RUN_SCHEMA && record["dataset"]["name"] == "maithic-library"
        @test record["classes"] == length(model.templates) && record["vocabulary"] == length(model.vocabulary)
        @test record["parameters"] == sum(length(array) for (_, array) in parameters(model))
        @test record["config"]["dim"] == model.config.dim && record["config"]["max_length"] == model.config.max_length
        candidates = ["omega", "simp [double]", "exact h", "rfl", "intro x"]
        priors = tactic_priors(model, "n : Nat\n⊢ double n = 2 * n", candidates)
        @test length(priors) == 5 && sum(priors) ≈ 1.0 && all(>(0.0), priors)
        @test 0.0 <= state_value(model, "n : Nat\n⊢ double n = 2 * n") <= 1.0
    end
end
