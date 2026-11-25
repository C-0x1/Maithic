# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

const UNIT = ["0 <= x", "x <= 1"]

box(pairs...) = Pair{String,Tuple{Exact,Exact}}[name => (Exact(lo), Exact(hi)) for (name, (lo, hi)) in pairs]
inside(value::Exact, e::RealInterval) = Exact(e.lo) <= value <= Exact(e.hi)

"A random expression in x and y that is defined everywhere: divisors and arguments of roots and logarithms are kept positive."
function random_real(rng::SplitMix, depth::Int)
    choice = depth == 0 ? integer!(rng, 1:2) : integer!(rng, 1:12)
    sub() = random_real(rng, depth - 1)
    positive() = BinOp(:+, Call(:abs, Node[sub()]), Num(Exact(1)))
    choice == 1 ? Num(Exact(integer!(rng, -5:5) // integer!(rng, 1:4))) :
    choice == 2 ? Var(("x", "y")[integer!(rng, 1:2)]) :
    choice == 3 ? Neg(sub()) :
    choice == 4 ? Pow(sub(), integer!(rng, 0:3)) :
    choice == 5 ? BinOp(:/, sub(), positive()) :
    choice == 6 ? Call(:sqrt, Node[positive()]) :
    choice == 7 ? Call(:log, Node[positive()]) :
    choice == 8 ? Call((:sin, :cos)[integer!(rng, 1:2)], Node[sub()]) :
    choice == 9 ? Call((:min, :max)[integer!(rng, 1:2)], Node[sub(), sub()]) :
    choice == 10 ? Call(:exp, Node[Call(:min, Node[sub(), Num(Exact(3))])]) :
    BinOp((:+, :-, :*)[integer!(rng, 1:3)], sub(), sub())
end

@testset "P13-05-01 intervals round outward and keep exact results exact" begin
    third = enclose(Float64, 1 // 3)
    @test third.lo < third.hi && nextfloat(third.lo) == third.hi && inside(Exact(1 // 3), third)
    @test enclose(Float64, 1 // 4) == RealInterval(0.25, 0.25)                 # representable: not widened
    @test enclose(Float64, 2, 3) + enclose(Float64, 5, 7) == RealInterval(7.0, 10.0)
    @test enclose(Float64, 2, 3) - enclose(Float64, 5, 7) == RealInterval(-5.0, -2.0)
    @test enclose(Float64, -2, 3) * enclose(Float64, 5, 7) == RealInterval(-14.0, 21.0)
    @test enclose(Float64, 1, 2) / enclose(Float64, 4, 8) == RealInterval(0.125, 0.5)
    @test enclose(Float64, -2, 3)^2 == RealInterval(0.0, 9.0) && enclose(Float64, -2, 3)^3 == RealInterval(-8.0, 27.0)
    @test enclose(Float64, -3, -2)^2 == RealInterval(4.0, 9.0) && enclose(Float64, -3, -2)^3 == RealInterval(-27.0, -8.0)
    @test enclose(Float64, -3, 2)^0 == RealInterval(1.0, 1.0)
    @test abs(enclose(Float64, -3, 2)) == RealInterval(0.0, 3.0) && abs(enclose(Float64, -3, -2)) == RealInterval(2.0, 3.0)
    @test min(enclose(Float64, 1, 5), enclose(Float64, 2, 3)) == RealInterval(1.0, 3.0)
    @test max(enclose(Float64, 1, 5), enclose(Float64, 2, 3)) == RealInterval(2.0, 5.0)
    @test sqrt(enclose(Float64, 4, 9)) == RealInterval(2.0, 3.0)
    @test exp(enclose(Float64, 0)) == RealInterval(1.0, 1.0) && log(enclose(Float64, 1)) == RealInterval(0.0, 0.0)
    @test sin(enclose(Float64, 0)) == RealInterval(0.0, 0.0) && cos(enclose(Float64, 0)) == RealInterval(1.0, 1.0)
    tenth = enclose(Float64, 1 // 10) + enclose(Float64, 2 // 10)
    @test tenth.lo < tenth.hi && inside(Exact(3 // 10), tenth)
    root = sqrt(enclose(Float64, 2))
    @test Exact(root.lo)^2 < 2 < Exact(root.hi)^2 && nextfloat(root.lo) == root.hi
    @test MaithicOracle.sum_bounds(1.0, 2.0^-60) == (1.0, nextfloat(1.0))
    @test MaithicOracle.sum_bounds(1.0, -(2.0^-60)) == (prevfloat(1.0), 1.0)
    @test MaithicOracle.sum_bounds(1.0e308, 1.0e308) == (floatmax(Float64), Inf)
    @test MaithicOracle.sum_bounds(-1.0e308, -1.0e308) == (-Inf, -floatmax(Float64))
    @test MaithicOracle.product_bounds(0.0, Inf) == (0.0, 0.0)
    @test MaithicOracle.product_bounds(1.0e-200, 1.0e-200) == (prevfloat(0.0), nextfloat(0.0))
    @test MaithicOracle.quotient_bounds(1.0, 3.0) == (1 / 3, nextfloat(1 / 3))
    @test MaithicOracle.quotient_bounds(1.0, Inf) == (0.0, 0.0) && MaithicOracle.quotient_bounds(6.0, -3.0) == (-2.0, -2.0)
    @test MaithicOracle.root_bounds(2.0) == (prevfloat(sqrt(2.0)), sqrt(2.0)) || MaithicOracle.root_bounds(2.0) == (sqrt(2.0), nextfloat(sqrt(2.0)))
    @test inv(RealInterval(2.0, Inf)) == RealInterval(0.0, 0.5)
    @test width(enclose(Float64, 1, 3)) == 2.0 && (2.0 in enclose(Float64, 1, 3)) && !(4.0 in enclose(Float64, 1, 3))
    @test issubset(enclose(Float64, 1, 2), enclose(Float64, 0, 3)) && !issubset(enclose(Float64, 0, 3), enclose(Float64, 1, 2))
    @test sprint(show, enclose(Float64, 1, 2)) == "[1.0, 2.0]"
    @test exact_bounds(enclose(Float64, 1 // 4, 1 // 2)) == (1 // 4, 1 // 2)
    @test_throws ArgumentError RealInterval(2.0, 1.0)
    @test_throws ArgumentError RealInterval(NaN, 1.0)
    @test_throws ArgumentError enclose(Float64, 1, 2)^(1 - 2)
    for T in (Float64, BigFloat)
        @test sin(enclose(T, 0, 2)) == RealInterval(T(0), T(1))            # the maximum at pi/2 is inside
        @test sin(enclose(T, 0, 7)) == RealInterval(T(-1), T(1))
        @test sin(enclose(T, 4, 5)).lo == -1 && sin(enclose(T, 4, 5)).hi < -0.75
        @test cos(enclose(T, 3, 4)).lo == -1 && cos(enclose(T, 3, 4)).hi < -0.65
        @test cos(enclose(T, -1, 1)).hi == 1 && 0.54 < cos(enclose(T, -1, 1)).lo < 0.5404
        @test sin(RealInterval(T(-Inf), T(0))) == RealInterval(T(-1), T(1))
        @test 0.84 < sin(enclose(T, 1, 3 // 2)).lo < sin(enclose(T, 1, 3 // 2)).hi < 0.998
        @test exp(enclose(T, -1000, -999)).lo >= 0
    end
    rng = SplitMix(7)
    rational() = Exact(integer!(rng, -2000:2000) // integer!(rng, 1:999))
    for _ in 1:400
        a, b, c, d = sort!([rational() for _ in 1:2])..., sort!([rational() for _ in 1:2])...
        A, B = enclose(Float64, a, b), enclose(Float64, c, d)
        x = a + (b - a) * integer!(rng, 0:8) // 8
        y = c + (d - c) * integer!(rng, 0:8) // 8
        @test inside(x + y, A + B) && inside(x - y, A - B) && inside(x * y, A * B)
        @test inside(x^2, A^2) && inside(x^3, A^3) && inside(x^5 * y^4, A^5 * B^4)
        @test inside(abs(x), abs(A)) && inside(min(x, y), min(A, B)) && inside(max(x, y), max(A, B))
        (c <= 0 <= d) ? (@test_throws Undefined A / B) : (@test inside(x / y, A / B))
    end
end

@testset "P13-05-02 real-valued goals are parsed with elementary functions" begin
    goal = read_goal("""{"schema": "maithic.oracle.goal/1", "kind": "real",
                         "hypotheses": ["0 <= x", "x <= 1"], "goal": "exp(x) * cos(x) <= 3 - sqrt(x) / 2"}""")
    @test goal.kind == :real && goal.domain == :real && goal.variables == ["x"]
    @test solve(goal) isa Proved
    @test_throws ArgumentError read_goal("""{"schema": "maithic.oracle.goal/1", "kind": "real", "goal": "tan(x) <= 3"}""")
    e = enclosure(parse_expression("sqrt(x) + exp(x - 4) + log(x / 4) + cos(x - 4) - 4"), box("x" => (4, 4)))
    @test e == RealInterval(0.0, 0.0)
    wide = enclosure(parse_expression("x * (1 - x)"), box("x" => (0, 1)))
    @test wide == RealInterval(0.0, 1.0)                    # the natural extension: sound, not tight
    @test enclosure(parse_expression("abs(x - y) + max(x, y) * min(x, 2)"), box("x" => (1, 2), "y" => (3, 5))) ==
          RealInterval(4.0, 14.0)
    big_e = enclosure(parse_expression("exp(1)"), box(), BigFloat)
    @test big_e.lo < big_e.hi && big_e.hi - big_e.lo < 1.0e-70 && big_e.lo < exp(big(1)) + 1.0e-70
    for (text, where) in (("1 / x", (-1, 1)), ("sqrt(x)", (-1, 1)), ("log(x)", (0, 1)), ("1 / (x - 1)", (1, 2)),
                          ("log(x - 2)", (1, 3)))
        @test_throws Undefined enclosure(parse_expression(text), box("x" => where))
    end
    @test occursin("contains zero", sprint(showerror, Undefined("a division by an interval that contains zero")))
    @test_throws UnboundVariable enclosure(parse_expression("x + z"), box("x" => (0, 1)))
end

@testset "P13-05-03 an enclosure that excludes violation proves the inequality" begin
    for goal in ("x * (1 - x) >= 0", "x * (1 - x) <= 1", "exp(x) < 3", "exp(x) != 0", "x - 2 != 0", "cos(x) > 1/2",
                 "sqrt(x + 1) <= 3/2", "log(x + 1) < 7/10", "x^2 + 1 >= 1")
        outcome = prove_real(UNIT, goal)
        @test outcome isa Proved && length(outcome.certificate.boxes) == 1 && check(outcome.certificate)
    end
    # Each occurrence of x is enclosed on its own, so x + 1 > x needs one cut before the enclosures separate.
    @test length(prove_real(UNIT, "x + 1 > x").certificate.boxes) == 2
    holds = MaithicOracle.certainly_holds
    @test holds(:le, -1, 0) && !holds(:le, -1, 1 // 10) && holds(:lt, -1, -1 // 10) && !holds(:lt, -1, 0)
    @test holds(:ge, 0, 1) && !holds(:ge, -1 // 10, 1) && holds(:gt, 1 // 10, 1) && !holds(:gt, 0, 1)
    @test holds(:ne, 1, 2) && holds(:ne, -2, -1) && !holds(:ne, -1, 1) && holds(:eq, 0, 0) && !holds(:eq, 0, 1)
    fails = MaithicOracle.certainly_fails
    @test fails(:le, 1, 2) && !fails(:le, 0, 2) && fails(:lt, 0, 2) && fails(:ge, -2, -1) && fails(:gt, -2, 0)
    @test fails(:eq, 1, 2) && fails(:ne, 0, 0) && !fails(:ne, 0, 1)
    @test prove_real(UNIT, "x = x") == Unknown("an equation cannot be proved by enclosures")
    @test prove_real(UNIT, "false") isa Unknown
    @test prove_real(["0 <= x"], "exp(x) >= 1") == Unknown("the variable x is not bounded on both sides by the hypotheses")
    @test prove_real(["1 <= x", "x <= 0"], "x > 5") isa Unknown
end

@testset "P13-05-04 the domain is subdivided until every box is decided" begin
    outcome = prove_real(UNIT, "x * (1 - x) <= 1/4 + 1/100")
    c = outcome.certificate
    @test length(c.boxes) == 18 && tiles(c.domain, c.boxes) && all(b.reason == 0 for b in c.boxes)
    @test c.domain == [(Exact(0), Exact(1))]
    @test sum(b.bounds[1][2] - b.bounds[1][1] for b in c.boxes) == 1
    @test prove_real(UNIT, "x * (1 - x) <= 1/4 + 1/100").certificate.boxes == c.boxes        # the same search every time
    two = prove_real(["0 <= x", "x <= 1", "0 <= y", "y <= 2"], "x * y - x^2 <= 1 + 1/50").certificate
    @test tiles(two.domain, two.boxes) && length(two.boxes) > 4 && check(two)
    @test sum((b.bounds[1][2] - b.bounds[1][1]) * (b.bounds[2][2] - b.bounds[2][1]) for b in two.boxes) == 2
    # A hypothesis that is not a bound removes the boxes on which it fails.
    disc = prove_real(["-1 <= x", "x <= 1", "-1 <= y", "y <= 1", "x^2 + y^2 <= 1/2"], "x + y <= 1.01").certificate
    @test check(disc) && any(b.reason == 5 for b in disc.boxes) && any(b.reason == 0 for b in disc.boxes)
    # A violated goal is refuted at the centre of a box, with exact coordinates.
    refuted = prove_real(UNIT, "x * (1 - x) <= 1/5")
    @test refuted == Refuted(["x" => Exact(1 // 2)])
    late = prove_real(["0 <= x", "x <= 4"], "sin(x) > -3/4")
    @test late isa Refuted && sin(Float64(late.model[1][2])) < -0.75
    tighter = prove_real(["0 <= x", "x <= 4", "x >= 3"], "sin(x) < 1/5")
    @test tighter isa Proved && tighter.certificate.domain == [(Exact(3), Exact(4))]      # bounds are intersected
    # The violated region reaches the domain in a corner; taking large boxes first finds it at once.
    @test prove_real(["-1 <= x", "x <= 1", "-1 <= y", "y <= 1"], "x + y <= 1.01") == Refuted(["x" => Exact(1), "y" => Exact(1)])
    @test prove_real(["-1 <= x", "x < 1", "-1 <= y", "y < 1"], "x + y <= 1.01") isa Refuted   # the corner itself is excluded
    # Equality in the interior cannot be settled by enclosures, and the budget says so.
    @test prove_real(["0 <= x", "x <= 2"], "x^2 - 2 * x + 1 >= 0"; max_boxes = 300) ==
          Unknown("undecided after 300 boxes; the goal may hold with equality somewhere, or need a finer method")
    @test prove_real(["2 <= x", "x <= 2"], "x * x >= 5") isa Refuted
    @test prove_real(["2 <= x", "x <= 2"], "x * x >= 4") isa Proved
    @test prove_real(["0 <= x", "x <= 1"], "1 / x > 0"; max_boxes = 50) isa Unknown       # undefined at the corner
    @test prove_real(["1/8 <= x", "x <= 1"], "1 / x > 0") isa Proved
    region = [(Exact(0), Exact(1)), (Exact(0), Exact(1))]
    quarter(x, y) = BoxClaim([(Exact(x), Exact(x) + 1 // 2), (Exact(y), Exact(y) + 1 // 2)], 0, (Exact(0), Exact(0)))
    quarters = [quarter(0, 0), quarter(0, 1 // 2), quarter(1 // 2, 0), quarter(1 // 2, 1 // 2)]
    @test tiles(region, quarters) && tiles(region, reverse(quarters))
    @test !tiles(region, quarters[1:3]) && !tiles(region, BoxClaim[]) && !tiles(region, [quarters; quarters[1:1]])
    @test !tiles(region, [BoxClaim([(Exact(0), Exact(1)), (Exact(0), Exact(2))], 0, (Exact(0), Exact(0)))])
    thirds = [BoxClaim([(Exact(0), Exact(1 // 3)), (Exact(0), Exact(1))], 0, (Exact(0), Exact(0))),
              BoxClaim([(Exact(1 // 3), Exact(1)), (Exact(0), Exact(1))], 0, (Exact(0), Exact(0)))]
    @test !tiles(region, thirds)                         # a cover, but not one made by halving
end

@testset "P13-05-05 the certificate lists boxes with rational enclosures" begin
    c = prove_real(UNIT, "x * (1 - x) <= 1/2").certificate
    @test kind(c) == "interval.boxes" && claim(c) == "∀ x : ℝ, 0 ≤ x → x ≤ 1 → x * (1 - x) ≤ 1 / 2"
    data = certificate_document(c)["data"]
    @test keys(data) == ["variables", "hypotheses", "goal", "domain", "boxes"]
    @test data["domain"] == Any[Any["0", "1"]] && length(data["boxes"]) == 2
    @test data["boxes"][1] == Object("bounds" => Any[Any["0", "1/2"]], "reason" => 0, "enclosure" => Any["-1/2", "0"])
    @test data["boxes"][2] == Object("bounds" => Any[Any["1/2", "1"]], "reason" => 0, "enclosure" => Any["-1/2", "0"])
    sizes = [b.bounds[1][2] - b.bounds[1][1] for b in prove_real(UNIT, "x * (1 - x) <= 1/4 + 1/100").certificate.boxes]
    @test issorted(sizes; rev = true) && first(sizes) == 1 // 4 && last(sizes) == 1 // 64      # larger boxes come first
    for certificate in (c, prove_real(UNIT, "x * (1 - x) <= 1/4 + 1/100").certificate,
                        prove_real(["-1 <= x", "x <= 1", "-1 <= y", "y <= 1", "x^2 + y^2 <= 1/2"], "x + y <= 1.01").certificate,
                        prove_real(["1/4 <= x", "x <= 4"], "sqrt(x) + 1 / x > 1.8").certificate,
                        prove_real(["0 <= x", "x <= 3", "0 <= y", "y <= 1"], "sin(x) * exp(-y) < 1.1 + y").certificate)
        @test round_trip(certificate)
        @test lean_check(certificate) === nothing        # real analysis is checked in the Mathlib tier
    end
    @test claim(prove_real(["1 <= x", "x <= 2"], "log(x) < x - 1/2").certificate) ==
          "∀ x : ℝ, 1 ≤ x → x ≤ 2 → (Real.log x) < x - 1 / 2"
    doc = outcome_document(prove_real(UNIT, "x * (1 - x) <= 1/5"))
    @test write_json(doc) == "{\"schema\":\"maithic.oracle.outcome/1\",\"outcome\":\"refuted\",\"model\":{\"x\":\"1/2\"}}"
end

@testset "P13-05-06 endpoints become short rationals that still enclose" begin
    accept_le(lo, hi) = hi <= 0
    @test simple_enclosure(-0.12345678, -0.0456789, accept_le) == (-124 // 1000, -45 // 1000)
    @test simple_enclosure(-0.5, -0.25, accept_le) == (-1 // 2, -1 // 4)
    @test simple_enclosure(-0.5, -0.0000004, accept_le) == (-1 // 2, 0 // 1)                 # zero is still not above zero
    strict = simple_enclosure(-0.5, -0.0000004, (lo, hi) -> hi < 0)                           # zero would not do here
    @test strict[1] == -1 // 2 && Exact(-0.0000004) <= strict[2] < 0 && denominator(strict[2]) <= 10^9
    tiny = simple_enclosure(-1.0, -1.0e-15, (lo, hi) -> hi < 0)
    @test tiny == (Exact(-1), Exact(-1.0e-15))                                                  # nothing shorter survives
    for (lo, hi) in ((0.1, 0.7), (-3.3333, 2.0000001), (1.0e-9, 2.0e-9), (-7.25, -7.25))
        short = simple_enclosure(lo, hi, (a, b) -> true)
        @test short[1] <= Exact(lo) && Exact(hi) <= short[2] && denominator(short[1]) <= 1000 && denominator(short[2]) <= 1000
    end
    c = prove_real(["1/4 <= x", "x <= 4"], "sqrt(x) + 1 / x > 1.8").certificate
    for claimed in c.boxes
        computed = MaithicOracle.difference_enclosure(c.goal, c.variables, claimed.bounds, Float64)
        @test claimed.enclosure[1] <= Exact(computed.lo) && Exact(computed.hi) <= claimed.enclosure[2]
        @test claimed.enclosure[1] > 0 && denominator(claimed.enclosure[1]) <= 10^12
    end
end

@testset "P13-05-07 certificates are verified again with independent arithmetic" begin
    good = prove_real(UNIT, "x * (1 - x) <= 1/4 + 1/100").certificate
    @test check(good) && check(good; T = Float64)
    tampered(; variables = good.variables, hypotheses = good.hypotheses, goal = good.goal, domain = good.domain,
             boxes = good.boxes) = IntervalCertificate(variables, hypotheses, goal, domain, boxes)
    first_box = good.boxes[1]
    narrowed = BoxClaim(first_box.bounds, 0, (first_box.enclosure[1] / 2, first_box.enclosure[2]))
    undecided = BoxClaim(first_box.bounds, 0, (first_box.enclosure[1], Exact(1)))
    wrong = [tampered(boxes = good.boxes[2:end]),                              # a piece of the domain is missing
             tampered(boxes = [good.boxes; good.boxes[1:1]]),
             tampered(boxes = [narrowed; good.boxes[2:end]]),                  # tighter than the truth
             tampered(boxes = [undecided; good.boxes[2:end]]),                 # does not decide the box
             tampered(boxes = [BoxClaim(first_box.bounds, 1, first_box.enclosure); good.boxes[2:end]]),
             tampered(boxes = [BoxClaim(first_box.bounds, 7, first_box.enclosure); good.boxes[2:end]]),
             tampered(boxes = [BoxClaim(good.domain, 0, (Exact(-1), Exact(0)))]),   # one box, claimed in one go
             tampered(domain = [(Exact(0), Exact(1 // 2))]),
             tampered(goal = parse_relation("x * (1 - x) <= 1/4 - 1/100")),
             tampered(goal = parse_relation("x * (1 - x) = 1/4")),
             tampered(hypotheses = good.hypotheses[1:1]),
             tampered(hypotheses = [good.hypotheses[1], Hypothesis("h2", parse_relation("x <= 2"))]),
             tampered(variables = ["x", "y"]),
             tampered(goal = parse_relation("1 / (x - 1/2) <= 5"))]
    for c in wrong
        @test !check(c)
        @test_throws RejectedCertificate certificate_document(c)
    end
    forged = replace(certificate_json(good), "\"goal\":\"x * (1 - x) <= 1 / 4 + 1 / 100\"" => "\"goal\":\"x * (1 - x) <= 1 / 4\"")
    @test forged != certificate_json(good) && !check(read_certificate(forged))
    @test_throws ArgumentError read_certificate(replace(certificate_json(good), "\"domain\":[[\"0\",\"1\"]]" => "\"domain\":[[\"0\"]]"))
end

@testset "P13-05-08 enclosures contain sampled values" begin
    rng = SplitMix(1234)
    checked = 0
    for _ in 1:400
        node = random_real(rng, 4)
        lows = [Exact(integer!(rng, -8:8) // integer!(rng, 1:4)) for _ in 1:2]
        widths = [Exact(integer!(rng, 0:6) // integer!(rng, 1:4)) for _ in 1:2]
        region = box("x" => (lows[1], lows[1] + widths[1]), "y" => (lows[2], lows[2] + widths[2]))
        fast = enclosure(node, region)
        slow = enclosure(node, region, BigFloat)
        (isfinite(fast.lo) && isfinite(fast.hi)) || continue
        checked += 1
        @test Exact(fast.lo) <= Exact(slow.lo) && Exact(slow.hi) <= Exact(fast.hi)     # more precision never widens
        for _ in 1:4
            point = [lows[i] + widths[i] * integer!(rng, 0:16) // 16 for i in 1:2]
            value = enclosure(node, box("x" => (point[1], point[1]), "y" => (point[2], point[2])), BigFloat)
            @test Exact(slow.lo) <= Exact(value.lo) && Exact(value.hi) <= Exact(slow.hi)
        end
    end
    @test checked > 300
    # Whole goals: what is proved passes the second check, what is refuted is refuted at its point.
    proved = refuted = 0
    for _ in 1:60
        node = random_real(rng, 3)
        margin = Exact(integer!(rng, -4:12) // 4)
        goal = Goal(:real, ["-1 <= x", "x <= 1", "0 <= y", "y <= 2"], Relation(:le, node, Num(margin)); domain = :real,
                    variables = ["x", "y"])
        outcome = prove_real(goal; max_boxes = 400)
        if outcome isa Proved
            proved += 1
            @test check(outcome.certificate) && check(outcome.certificate; T = Float64) && round_trip(outcome.certificate)
        elseif outcome isa Refuted
            refuted += 1
            @test MaithicOracle.refutes_at(goal, Exact[v for (_, v) in outcome.model])
        end
    end
    @test proved > 5 && refuted > 5
end
