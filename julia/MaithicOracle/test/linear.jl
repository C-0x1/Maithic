# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"Decides a system of rows by Fourier-Motzkin elimination: a slow method that shares nothing with the simplex code."
function eliminate(rows::Vector{Row}, width::Int)
    system = Tuple{Vector{Exact},Bool,Exact}[]
    for r in rows
        if r.op === :eq
            push!(system, (r.coefficients, false, r.bound), (-r.coefficients, false, -r.bound))
        else
            push!(system, (r.coefficients, r.op === :lt, r.bound))
        end
    end
    for j in 1:width
        next = [s for s in system if iszero(s[1][j])]
        for u in system, l in system
            (u[1][j] > 0 && l[1][j] < 0) || continue
            cu, cl = 1 / u[1][j], -1 / l[1][j]
            push!(next, (u[1] .* cu .+ l[1] .* cl, u[2] || l[2], u[3] * cu + l[3] * cl))
        end
        system = next
    end
    all(s -> s[2] ? 0 < s[3] : 0 <= s[3], system)
end

satisfies(row::Row, point) = begin
    value = sum(row.coefficients .* point; init = Exact(0))
    row.op === :eq ? value == row.bound : row.op === :le ? value <= row.bound : value < row.bound
end

function random_rows(rng::SplitMix, width::Int, count::Int)
    Row[Row(Exact[integer!(rng, -3:3) for _ in 1:width], (:le, :le, :le, :lt, :lt, :eq)[integer!(rng, 1:6)],
            Exact(integer!(rng, -5:5) // integer!(rng, 1:2))) for _ in 1:count]
end

model_of(outcome::Refuted) = Dict(outcome.model)

@testset "P13-02-01 linear goals are read from the exchange format" begin
    goal = read_goal("""
        {"schema": "maithic.oracle.goal/1", "kind": "linear", "domain": "rat",
         "hypotheses": [{"name": "budget", "statement": "2*x + 3*y <= 6"}, "x >= 0", {"statement": "0 <= y"}],
         "goal": "x + y <= 3"}""")
    @test goal.kind == :linear && goal.domain == :rat && goal.variables == ["x", "y"]
    @test [h.name for h in goal.hypotheses] == ["budget", "h2", "h3"]
    @test goal.goal == parse_relation("x + y <= 3")
    @test read_goal(write_json(goal_document(goal))) == goal
    @test read_goal("{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"false\"}").goal === nothing
    ordered = read_goal("{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"variables\":[\"y\",\"x\",\"z\"],\"goal\":\"x<y\"}")
    @test ordered.variables == ["y", "x", "z"]
    for bad in ("{\"kind\":\"linear\",\"goal\":\"x<1\"}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"cubic\",\"goal\":\"x<1\"}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"domain\":\"complex\",\"goal\":\"x<1\"}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\"}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x <\"}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"variables\":[\"y\"]}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"variables\":[\"x\",\"x\"]}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"hypotheses\":[\"x +\"]}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"hypotheses\":[{\"name\":\"a\"}]}",
                "{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"hypotheses\":" *
                "[{\"name\":\"a\",\"statement\":\"x>0\"},{\"name\":\"a\",\"statement\":\"x>1\"}]}")
        @test_throws ArgumentError read_goal(bad)
    end
    message = try
        read_goal("{\"schema\":\"maithic.oracle.goal/1\",\"kind\":\"linear\",\"goal\":\"x<1\",\"hypotheses\":[{\"name\":\"hx\",\"statement\":\"x ? 1\"}]}")
    catch e
        sprint(showerror, e)
    end
    @test occursin("hypothesis hx", message) && occursin("character 3", message)

    form = linear_form(parse_expression("2 * (x - 3 * y) / 4 + x * 0.5 - (1 + y) + 2^3 * z^1 + z^0"))
    @test form == LinearForm(Dict("x" => Exact(1), "y" => Exact(-5 // 2), "z" => Exact(8)), Exact(0))
    @test linear_form(parse_expression("x - x")) == LinearForm(Dict{String,Exact}(), Exact(0))
    for nonlinear in ("x * y", "x^2", "1 / x", "sqrt(x)", "x / 0", "abs(x)")
        @test_throws NotLinear linear_form(parse_expression(nonlinear))
    end
    @test occursin("not linear", sprint(showerror, NotLinear("why")))
    @test linear_row(parse_relation("2*x + 1 >= y - 3"), ["x", "y"], :rat) == Row(Exact[-2, 1], :le, Exact(4))
    @test linear_row(parse_relation("x < y"), ["y", "x"], :rat) == Row(Exact[-1, 1], :lt, Exact(0))
    @test linear_row(parse_relation("x = 2"), ["x"], :rat) == Row(Exact[1], :eq, Exact(2))
    @test_throws NotLinear linear_row(parse_relation("x != 2"), ["x"], :rat)
    @test_throws ArgumentError linear_row(parse_relation("x = z"), ["x"], :rat)
end

@testset "P13-02-02 the simplex method is exact" begin
    # maximize 3a + 5b subject to a <= 4, 2b <= 12, 3a + 2b <= 18: the optimum is 36 at (2, 6).
    M = Exact[1 0 1 0 0; 0 2 0 1 0; 3 2 0 0 1]
    r = Exact[4, 12, 18]
    c = Exact[-3, -5, 0, 0, 0]
    result = simplex(M, r, c)
    @test result.status == :optimal && result.value == -36 && result.solution[1:2] == [2, 6]
    @test sum(result.duals .* r) == result.value                       # strong duality, exactly
    @test all(result.duals' * M .<= c')                                # dual feasibility
    @test result.duals == Exact[0, -3 // 2, -1]
    # An optimum that is not an integer.
    fraction = simplex(Exact[3 1 1 0; 1 2 0 1], Exact[7, 5], Exact[-1, -1, 0, 0])
    @test fraction.value == -17 // 5 && fraction.solution[1:2] == [9 // 5, 8 // 5]
    # No feasible point: a + b = 1 and a + b = 3. The duals say why.
    none = simplex(Exact[1 1; 1 1], Exact[1, 3], Exact[0, 0])
    @test none.status == :infeasible
    @test all(none.duals' * Exact[1 1; 1 1] .<= 0) && sum(none.duals .* Exact[1, 3]) > 0
    # Unbounded: minimize -a with a - b = 0.
    @test simplex(Exact[1 -1], Exact[0], Exact[-1, 0]).status == :unbounded
    # A negative right side is handled by turning the row around, and the duals keep their orientation.
    turned = simplex(Exact[-1 -1 1], Exact[-2], Exact[1, 2, 0])
    @test turned.status == :optimal && turned.value == 2 && turned.duals == Exact[-1]
    @test all(turned.duals' * Exact[-1 -1 1] .<= Exact[1 2 0])
    @test simplex(Exact[-1 -1], Exact[-2], Exact[1, 2]).value == 2
    # A redundant row leaves an artificial variable in the basis; it must do no harm.
    redundant = simplex(Exact[1 1; 2 2], Exact[2, 4], Exact[1, 3])
    @test redundant.status == :optimal && redundant.value == 2 && redundant.solution == [2, 0]
    # Beale's example cycles under the textbook pivot rule; Bland's rule ends it.
    beale = simplex(Exact[1//4 -8 -1 9 1 0 0; 1//2 -12 -1//2 3 0 1 0; 0 0 1 0 0 0 1], Exact[0, 0, 1],
                    Exact[-3 // 4, 20, -1 // 2, 6, 0, 0, 0])
    @test beale.status == :optimal && beale.value == -5 // 4
    @test simplex(zeros(Exact, 0, 2), Exact[], Exact[1, 1]).value == 0
    @test_throws DimensionMismatch simplex(Exact[1 1], Exact[1, 2], Exact[0, 0])
    rng = SplitMix(31)
    for _ in 1:200
        rows, columns = integer!(rng, 1:4), integer!(rng, 2:6)
        M = Exact[integer!(rng, -3:3) for _ in 1:rows, _ in 1:columns]
        r = Exact[integer!(rng, -4:4) for _ in 1:rows]
        c = Exact[integer!(rng, -3:3) for _ in 1:columns]
        result = simplex(M, r, c)
        if result.status == :optimal
            @test M * result.solution == r && all(result.solution .>= 0)
            @test sum(c .* result.solution) == result.value == sum(result.duals .* r)
            @test all(result.duals' * M .<= c')
        elseif result.status == :infeasible
            @test all(result.duals' * M .<= 0) && sum(result.duals .* r) > 0
        end
    end
end

@testset "P13-02-03 Farkas multipliers refute contradictory hypotheses" begin
    outcome = prove_linear(["x + y <= 1", "x >= 1", "y >= 1"], "false")
    @test outcome isa Proved && kind(outcome.certificate) == "linear.farkas"
    c = outcome.certificate
    @test c.refutations == [Refutation(nothing, Exact[1, 1, 1])] && check(c)
    @test claim(c) == "∀ x y : ℚ, x + y ≤ 1 → x ≥ 1 → y ≥ 1 → False"
    strict = prove_linear(["x < 1", "x >= 1"], "false").certificate
    @test strict.refutations[1].multipliers == Exact[1, 1] && check(strict)
    equations = prove_linear(["x + y = 2", "x - y = 0", "x = 2"], "false").certificate
    @test check(equations) && any(<(0), equations.refutations[1].multipliers)   # an equation may be subtracted
    @test check(prove_linear(["0 * x >= 1"], "false").certificate)
    @test check(prove_linear(["x < x"], "false").certificate)
    scaled = prove_linear(["x/3 + y/7 >= 1", "x <= 1", "y <= 4"], "false").certificate
    @test check(scaled) && all(m -> m isa Exact, scaled.refutations[1].multipliers)
    satisfiable = prove_linear(["x + y <= 1", "x >= 1"], "false")
    @test satisfiable isa Refuted && model_of(satisfiable)["x"] + model_of(satisfiable)["y"] <= 1
    @test prove_linear(String[], "false") == Refuted(Pair{String,Exact}[])
    @test prove_linear(["x * y <= 1"], "false") == Unknown("hypothesis h1 is not linear: two variable terms are multiplied")
    @test prove_linear(["x != 1"], "false") isa Unknown
end

@testset "P13-02-04 goals follow as combinations of hypotheses" begin
    outcome = prove_linear(["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 3")
    c = outcome.certificate
    @test kind(c) == "linear.combination" && check(c)
    @test c.refutations[1].assumption == parse_relation("x + y > 3")
    @test claim(c) == "∀ x y : ℚ, 2 * x + 3 * y ≤ 6 → x ≥ 0 → y ≥ 0 → x + y ≤ 3"
    @test last(c.refutations[1].multipliers) > 0                  # the negated goal takes part in the contradiction
    equation = prove_linear(["x + y = 10", "x - y = 2"], "x = 6").certificate
    @test length(equation.refutations) == 2 && check(equation)
    @test [r.assumption for r in equation.refutations] == [parse_relation("x < 6"), parse_relation("x > 6")]
    distinct = prove_linear(["x > y"], "x != y").certificate
    @test distinct.refutations[1].assumption == parse_relation("x = y") && check(distinct)
    @test check(prove_linear(["x > 0"], "2 * x > x").certificate)
    @test check(prove_linear(String[], "x + 1 > x").certificate)
    transitive = prove_linear(["a <= b", "b <= c", "c <= d", "d <= e"], "a <= e").certificate
    @test check(transitive) && transitive.refutations[1].multipliers == Exact[1, 1, 1, 1, 1]
    refuted = prove_linear(["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 2")
    @test refuted isa Refuted
    point = model_of(refuted)
    @test 2 * point["x"] + 3 * point["y"] <= 6 && point["x"] >= 0 && point["y"] >= 0 && point["x"] + point["y"] > 2
    @test prove_linear(["x + y = 10"], "x = 6") isa Refuted
    @test prove_linear(["x >= 0"], "x^2 >= 0") == Unknown("the goal is not linear: a variable term is raised to the power 2")
end

@testset "P13-02-05 certificates are checked before they are emitted" begin
    rows = Row[Row(Exact[1, 1], :le, Exact(1)), Row(Exact[-1, 0], :le, Exact(-1)), Row(Exact[0, -1], :le, Exact(-1))]
    @test refutes(rows, Exact[1, 1, 1])
    @test refutes(rows, Exact[2, 2, 2])
    @test !refutes(rows, Exact[1, 1, 0])               # the left sides do not cancel
    @test !refutes(rows, Exact[0, 0, 0])               # 0 <= 0 is no contradiction
    @test !refutes(rows, Exact[-1, -1, -1])            # an inequality cannot be subtracted
    @test !refutes(rows, Exact[1, 1])
    @test !refutes(Row[], Exact[])
    @test !refutes(Row[Row(Exact[1], :le, Exact(0)), Row(Exact[-1], :le, Exact(0))], Exact[1, 1])     # x = 0 is fine
    @test refutes(Row[Row(Exact[1], :lt, Exact(0)), Row(Exact[-1], :le, Exact(0))], Exact[1, 1])      # 0 < 0
    @test !refutes(Row[Row(Exact[1], :lt, Exact(0)), Row(Exact[-1], :le, Exact(0))], Exact[0, 0])
    @test refutes(Row[Row(Exact[1], :eq, Exact(1)), Row(Exact[1], :eq, Exact(2))], Exact[1, -1])
    @test !refutes(Row[Row(Exact[1], :le, Exact(1)), Row(Exact[1, 2], :le, Exact(2))], Exact[1, 1])

    good = prove_linear(["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 3").certificate
    tampered(; domain = good.domain, variables = good.variables, hypotheses = good.hypotheses, goal = good.goal,
             refutations = good.refutations) = LinearCertificate(domain, variables, hypotheses, goal, refutations)
    wrong = [tampered(refutations = [Refutation(good.refutations[1].assumption, Exact[1, 1, 1, 1])]),
             tampered(refutations = [Refutation(parse_relation("x + y >= 3"), good.refutations[1].multipliers)]),
             tampered(refutations = [Refutation(nothing, good.refutations[1].multipliers[1:3])]),
             tampered(refutations = Refutation[]),
             tampered(goal = parse_relation("x + y <= 2")),
             tampered(goal = parse_relation("x + y = 3")),
             tampered(hypotheses = good.hypotheses[1:2]),
             tampered(hypotheses = [good.hypotheses[1], good.hypotheses[1], good.hypotheses[3]]),
             tampered(variables = ["x"]),
             tampered(variables = ["x", "x"]),
             tampered(domain = :real),
             tampered(goal = parse_relation("x * y <= 3"))]
    for c in wrong
        @test !check(c)
        @test_throws RejectedCertificate certificate_document(c)
    end
    @test check(good)
end

@testset "P13-02-06 linear certificates are written in the shared schema" begin
    c = prove_linear(["x + y <= 1", "x >= 1", "y >= 1/2"], "false").certificate
    @test certificate_json(c) ==
          "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"linear.farkas\"," *
          "\"claim\":\"∀ x y : ℚ, x + y ≤ 1 → x ≥ 1 → y ≥ 1 / 2 → False\"," *
          "\"data\":{\"domain\":\"rat\",\"variables\":[\"x\",\"y\"],\"hypotheses\":[" *
          "{\"name\":\"h1\",\"statement\":\"x + y <= 1\"},{\"name\":\"h2\",\"statement\":\"x >= 1\"}," *
          "{\"name\":\"h3\",\"statement\":\"y >= 1 / 2\"}],\"goal\":\"false\"," *
          "\"refutations\":[{\"assumption\":null,\"multipliers\":[\"1\",\"1\",\"1\"]}]}," *
          "\"engine\":{\"name\":\"MaithicOracle\",\"version\":\"0.1.0\",\"parameters\":{}}}"
    for certificate in (c, prove_linear(["x + y = 10", "x - y = 2"], "x = 6").certificate,
                        prove_linear(["x/3 + y/7 >= 1", "x <= 1", "y <= 4"], "false").certificate,
                        prove_linear(["3*x + 5*y < 7", "x >= 1"], "y <= 0"; domain = :int).certificate)
        @test round_trip(certificate)
    end
    doc = outcome_document(prove_linear(["x >= 1"], "x > 0"))
    @test doc["schema"] == "maithic.oracle.outcome/1" && doc["outcome"] == "proved"
    @test doc["certificate"]["kind"] == "linear.combination"
    refuted = outcome_document(prove_linear(["x >= 1"], "x > 1"))
    @test write_json(refuted) == "{\"schema\":\"maithic.oracle.outcome/1\",\"outcome\":\"refuted\",\"model\":{\"x\":\"1\"}}"
    @test sprint(show, prove_linear(["x >= 1"], "x > 1")) == "Refuted(x = 1)"
    @test sprint(show, prove_linear(["2 * x = 1", "y = x"], "false")) == "Refuted(x = 1/2, y = 1/2)"
    @test sprint(show, prove_linear(["x >= 1"], "x > 0")) == "Proved(linear.combination: ∀ x : ℚ, x ≥ 1 → x > 0)"
    @test sprint(show, prove_linear(["x * x >= 1"], "x > 1")) ==
          "Unknown(\"hypothesis h1 is not linear: two variable terms are multiplied\")"
    unknown = outcome_document(prove_linear(["x * x >= 1"], "x > 1"))
    @test unknown["outcome"] == "unknown" && occursin("not linear", unknown["reason"])
    @test lean_check(prove_linear(["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 3").certificate) ==
          "example (x y : Rat) (h1 : (2 * x + 3 * y : Rat) ≤ 6) (h2 : (x : Rat) ≥ 0) (h3 : (y : Rat) ≥ 0) : " *
          "(x + y : Rat) ≤ 3 := by grind\n"
    @test lean_check(prove_linear(["x > 0"], "2 * x >= 2"; domain = :int).certificate) ==
          "example (x : Int) (h1 : (x : Int) > 0) : (2 * x : Int) ≥ 2 := by omega\n"
    @test lean_check(prove_linear(["x / 2 > 0"], "x >= 1"; domain = :int).certificate) === nothing
    named = prove_linear(Goal(:linear, [Hypothesis("not a name", parse_relation("x < 0"))], "x < 1")).certificate
    @test occursin("(_ : (x : Rat) < 0)", lean_check(named))
end

@testset "P13-02-09 certificates over the integers are written out for the Lean checker" begin
    gap = prove_linear(["2*x + 4*y < 7", "x + 2*y >= 4"], "false"; domain = :int).certificate
    @test lean_replay(gap) ==
          "example (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7) (h2 : (x + 2 * y : Int) ≥ 4) : False := by\n" *
          "  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h1, h2]\n"
    # An equation fails in two ways, and each has its own refutation.
    parity = lean_replay(prove_linear(["2*x + 4*y < 7", "x + 2*y > 2"], "x + 2*y = 3"; domain = :int).certificate; name = "parity")
    @test startswith(parity, "theorem parity (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7)")
    @test count("maithic_linear_int", parity) == 2
    @test occursin("  rcases Int.lt_or_gt_of_ne negated with negated | negated\n  · maithic_linear_int", parity)
    # An inequality is negated once; a goal with ≠ is an implication already.
    bound = lean_replay(prove_linear(["3*x >= 4", "y >= x"], "y >= 2"; domain = :int).certificate)
    @test occursin("  apply Classical.byContradiction\n  intro negated\n  maithic_linear_int", bound)
    @test endswith(bound, "using [h1, h2, negated]\n")
    distinct = lean_replay(prove_linear(["2*x + 1 = 2*y"], "x != y"; domain = :int).certificate)
    @test occursin(": (x : Int) ≠ y := by\n  intro negated\n", distinct) && !occursin("byContradiction", distinct)
    # A row with multiplier zero is left out, with its hypothesis: here the negated goal is not needed.
    @test endswith(distinct, "[(1, ⟨[0, 0], .le, -1⟩)] using [h1]\n")
    # Multipliers are written as whole numbers.
    halves = LinearCertificate(:int, ["x"], [Hypothesis("h1", parse_relation("2 * x <= 1")),
                                             Hypothesis("h2", parse_relation("3 * x >= 1"))],
                               nothing, [Refutation(nothing, Exact[1 // 2, 1 // 2])])
    @test check(halves)
    @test occursin("[(1, ⟨[1], .le, 0⟩), (1, ⟨[-1], .le, -1⟩)] using [h1, h2]", lean_replay(halves))
    # Nothing is written where the Lean checker does not apply, or for a certificate that fails its check.
    @test lean_replay(prove_linear(["x + y <= 1", "x >= 1", "y >= 1/2"], "false").certificate) === nothing
    @test lean_replay(prove_linear(["x / 2 > 0"], "x >= 1"; domain = :int).certificate) === nothing
    @test lean_replay(prove_prime(97)) === nothing
    broken = LinearCertificate(:int, gap.variables, gap.hypotheses, nothing, [Refutation(nothing, Exact[1, 2])])
    @test_throws RejectedCertificate lean_replay(broken)
    @test_throws ArgumentError lean_replay(gap; name = "not a name")
    # Hypotheses whose names Lean cannot use get one, and the name of the negated goal avoids the others.
    renamed = prove_linear(Goal(:linear, [Hypothesis("not a name", parse_relation("x < 0")),
                                          Hypothesis("negated", parse_relation("x >= 0"))], "false"; domain = :int)).certificate
    @test occursin("(hypothesis_1 : (x : Int) < 0) (negated : (x : Int) ≥ 0)", lean_replay(renamed))
    clash = lean_replay(prove_linear(Goal(:linear, [Hypothesis("negated", parse_relation("x > 0"))], "x >= 1";
                                          domain = :int)).certificate)
    @test occursin("  intro negated'\n", clash) && endswith(clash, "using [negated, negated']\n")
    @test LEAN_REPLAY_IMPORT == "import Maithic.Oracle.Logic.LinearIntTactic"
    # Names as Lean writes them, with subscripts and letters of other alphabets, are kept.
    subscripts = prove_linear(Goal(:linear, [Hypothesis("h₁", parse_relation("x < 0")), Hypothesis("hα'", parse_relation("x >= 0"))],
                                   "false"; domain = :int)).certificate
    @test endswith(lean_replay(subscripts), "using [h₁, hα']\n")
end

@testset "P13-01-05 a goal document is answered with an outcome that Lean can check" begin
    gap = """{"schema": "maithic.oracle.goal/1", "kind": "linear", "domain": "int",
              "hypotheses": [{"name": "h1", "statement": "2*x + 4*y < 7"}, {"name": "h2", "statement": "x + 2*y >= 4"}],
              "goal": "false"}"""
    doc = read_json(answer(gap))
    @test doc["schema"] == "maithic.oracle.outcome/1" && doc["outcome"] == "proved"
    @test check(read_certificate(doc["certificate"]))
    # The scripts close the goal inside its own theorem: the replay of the certificate first, then the decision procedure.
    @test doc["lean"]["scripts"] ==
          ["maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h1, h2]", "omega"]
    @test doc["lean"]["replay"] == lean_replay(read_certificate(doc["certificate"]))
    @test doc["lean"]["check"] == lean_check(read_certificate(doc["certificate"]))
    # A goal with two cases keeps the structure of its script.
    parity = lean_scripts(prove_linear(["2*x + 4*y < 7", "x + 2*y > 2"], "x + 2*y = 3"; domain = :int).certificate)
    @test startswith(parity[1], "apply Classical.byContradiction\nintro negated\nrcases Int.lt_or_gt_of_ne negated with negated | negated\n· maithic_linear_int")
    @test parity[2] == "omega"
    # Over the rationals there is no checker to replay through, only the decision procedure.
    @test lean_scripts(prove_linear(["x + y <= 1", "x >= 1", "y >= 1/2"], "false").certificate) == ["grind"]
    @test lean_scripts(prove_prime(97)) == String[]
    # Refuted and unknown outcomes say nothing about Lean.
    refuted = read_json(answer("""{"schema": "maithic.oracle.goal/1", "kind": "linear", "hypotheses": ["x >= 0"], "goal": "x >= 1"}"""))
    @test refuted["outcome"] == "refuted" && refuted["model"]["x"] == "0" && !haskey(refuted, "lean")
    unknown = read_json(answer("""{"schema": "maithic.oracle.goal/1", "kind": "linear", "hypotheses": ["x * x >= 1"], "goal": "x > 1"}"""))
    @test unknown["outcome"] == "unknown" && !haskey(unknown, "lean")
    @test_throws ArgumentError answer("""{"schema": "maithic.oracle.goal/1", "kind": "linear"}""")
    # The documents written for other languages are unchanged unless the members for Lean are asked for.
    @test !haskey(outcome_document(prove_linear(["x >= 1"], "x > 0")), "lean")
    # The program that the command runs prints the same answer.
    script = joinpath(@__DIR__, "..", "bin", "solve.jl")
    printed = read(pipeline(IOBuffer(gap), `$(Base.julia_cmd()) --startup-file=no --project=$(dirname(@__DIR__)) $script`), String)
    @test read_json(printed) == doc
end

@testset "P13-02-07 strict inequalities are strengthened over the integers" begin
    @test tighten(Row(Exact[2, 4], :lt, Exact(7))) == Row(Exact[1, 2], :le, Exact(3))
    @test tighten(Row(Exact[2, 4], :le, Exact(7))) == Row(Exact[1, 2], :le, Exact(3))
    @test tighten(Row(Exact[2, 4], :lt, Exact(8))) == Row(Exact[1, 2], :le, Exact(3))
    @test tighten(Row(Exact[1 // 2, 1 // 3], :lt, Exact(1))) == Row(Exact[3, 2], :le, Exact(5))
    @test tighten(Row(Exact[-3], :le, Exact(-7))) == Row(Exact[-1], :le, Exact(-3))
    @test tighten(Row(Exact[2, 2], :eq, Exact(4))) == Row(Exact[1, 1], :eq, Exact(2))
    @test tighten(Row(Exact[2, 2], :eq, Exact(3))) == Row(Exact[0, 0], :le, Exact(-1))
    @test tighten(Row(Exact[0, 0], :lt, Exact(1))) == Row(Exact[0, 0], :lt, Exact(1))
    @test linear_row(parse_relation("x > 0"), ["x"], :int) == Row(Exact[-1], :le, Exact(-1))

    @test prove_linear(["x > 0"], "x >= 1"; domain = :int) isa Proved
    @test prove_linear(["x > 0"], "x >= 1"; domain = :rat) isa Refuted
    @test prove_linear(["2 * x = 1"], "false"; domain = :int) isa Proved
    @test prove_linear(["2 * x = 1"], "false"; domain = :rat) == Refuted(["x" => Exact(1 // 2)])
    parity = prove_linear(["2*x + 4*y < 7", "x + 2*y > 2"], "x + 2*y = 3"; domain = :int)
    @test parity isa Proved && check(parity.certificate) && parity.certificate.domain == :int
    @test claim(parity.certificate) == "∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y > 2 → x + 2 * y = 3"
    @test prove_linear(["2*x + 4*y < 7", "x + 2*y > 2"], "x + 2*y = 3"; domain = :rat) isa Refuted
    # The relaxation has a fractional vertex; an integer counterexample is found by search.
    searched = prove_linear(["5*x + 7*y = 3"], "false"; domain = :int)
    @test searched isa Refuted && all(isinteger(v) for (_, v) in searched.model)
    @test 5 * model_of(searched)["x"] + 7 * model_of(searched)["y"] == 3
    # Rational solutions exist, integer ones do not, and no single row shows it: the oracle does not know.
    @test prove_linear(["x + y = 1", "x - y = 0"], "false"; domain = :int) isa Unknown
    @test prove_linear(["x + y = 1", "x - y = 0"], "false"; domain = :rat) == Refuted(["x" => Exact(1 // 2), "y" => Exact(1 // 2)])
    int_certificate = prove_linear(["3*x + 5*y < 7", "x >= 1"], "y <= 0"; domain = :int).certificate
    @test check(int_certificate)
    as_rational = LinearCertificate(:rat, int_certificate.variables, int_certificate.hypotheses, int_certificate.goal,
                                    int_certificate.refutations)
    @test !check(as_rational)                         # the multipliers belong to the tightened rows
end

@testset "P13-02-08 random feasible and infeasible systems" begin
    rng = SplitMix(77)
    feasible = infeasible = 0
    for _ in 1:600
        width = integer!(rng, 1:3)
        rows = random_rows(rng, width, integer!(rng, 1:6))
        status, values = feasibility(rows, width)
        @test (status == :feasible) == eliminate(rows, width)
        if status == :feasible
            feasible += 1
            @test all(satisfies(row, values) for row in rows)
        else
            infeasible += 1
            @test refutes(rows, values)
        end
    end
    @test feasible > 100 && infeasible > 100
    # Feasible by construction: every row is satisfied by a chosen point.
    for _ in 1:150
        width = integer!(rng, 1:4)
        point = Exact[integer!(rng, -4:4) // integer!(rng, 1:3) for _ in 1:width]
        rows = Row[]
        for _ in 1:integer!(rng, 1:6)
            a = Exact[integer!(rng, -3:3) for _ in 1:width]
            slack = integer!(rng, 0:2)
            push!(rows, slack == 0 ? Row(a, :eq, sum(a .* point)) :
                        Row(a, (:le, :lt)[slack], sum(a .* point) + slack - 1 // 2))
        end
        status, values = feasibility(rows, width)
        @test status == :feasible && all(satisfies(row, values) for row in rows)
    end
    # Infeasible by construction: two rows that contradict each other, hidden among others.
    for _ in 1:150
        width = integer!(rng, 1:4)
        a = Exact[integer!(rng, -3:3) for _ in 1:width]
        all(iszero, a) && (a[1] = 1)
        bound = Exact(integer!(rng, -5:5))
        rows = random_rows(rng, width, integer!(rng, 0:4))
        push!(rows, Row(a, :le, bound), Row(-a, (:le, :lt)[integer!(rng, 1:2)], -bound - integer!(rng, 0:2)))
        last(rows).op === :le && last(rows).bound == -bound && (rows[end] = Row(-a, :lt, -bound))
        MaithicCore.shuffle!(rng, rows)
        status, multipliers = feasibility(rows, width)
        @test status == :infeasible && refutes(rows, multipliers)
    end
    # Whole goals: whatever the oracle says is backed by a certificate or a model that checks.
    names = ["x", "y", "z"]
    text(row) = begin
        left = join(("$(numerator(c)) * $(n)" for (c, n) in zip(row.coefficients, names)), " + ")
        "$(left) $(row.op === :eq ? "=" : row.op === :le ? "<=" : "<") $(exact_text(row.bound))"
    end
    proved = refuted = 0
    for _ in 1:300
        width = integer!(rng, 1:3)
        rows = random_rows(rng, width, integer!(rng, 1:5))
        goal = Goal(:linear, text.(rows[1:(end - 1)]), text(last(rows)); variables = names[1:width])
        outcome = prove_linear(goal)
        if outcome isa Proved
            proved += 1
            @test check(outcome.certificate) && round_trip(outcome.certificate)
        else
            refuted += 1
            @test outcome isa Refuted && MaithicOracle.is_counterexample(goal, outcome.model)
        end
    end
    @test proved > 20 && refuted > 20
end
