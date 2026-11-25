# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

const XY = ["x", "y"]
const XYZ = ["x", "y", "z"]

poly(text, variables = XY) = polynomial(parse_expression(text), variables)

"The leading monomials of a basis that no other leading monomial divides: an invariant of the ideal."
function minimal_leading(basis)
    leading = unique([MaithicOracle.leading_monomial(b.polynomial) for b in basis])
    sort([m for m in leading if !any(o -> o != m && all(o .<= m), leading)])
end

"Buchberger's criterion, checked directly: every S-polynomial of the basis reduces to zero."
function is_groebner(basis)
    polys = [b.polynomial for b in basis]
    for i in eachindex(polys), j in (i + 1):length(polys)
        mi, mj = MaithicOracle.leading_monomial(polys[i]), MaithicOracle.leading_monomial(polys[j])
        common = max.(mi, mj)
        s = MaithicOracle.shifted(polys[i], Exact(1), common .- mi) - MaithicOracle.shifted(polys[j], Exact(1), common .- mj)
        iszero(divide(s, polys)[2]) || return false
    end
    true
end

function random_polynomial(rng::SplitMix, width::Int, terms::Int, max_degree::Int)
    coefficients = Dict{Monomial,Exact}()
    for _ in 1:terms
        m = zeros(Int, width)
        for _ in 1:integer!(rng, 0:max_degree)
            m[integer!(rng, 1:width)] += 1
        end
        coefficients[m] = Exact(integer!(rng, -4:4) // integer!(rng, 1:3))
    end
    MaithicOracle.normal(width, coefficients)
end

@testset "P13-03-01 polynomial goals are parsed" begin
    @test poly("(x + y)^2").terms == [([2, 0], Exact(1)), ([1, 1], Exact(2)), ([0, 2], Exact(1))]
    @test poly("x * y - y * x") == poly("0") && iszero(poly("0"))
    @test poly("(x + 1) / 2 - 0.5") == poly("x / 2")
    @test poly("x^0") == poly("1") && poly("-(x - y)") == poly("y - x")
    @test poly("x", ["y", "x"]).terms == [([0, 1], Exact(1))]
    for outside in ("sqrt(x)", "x / y", "1 / (x - x)", "abs(x)")
        @test_throws NotPolynomial poly(outside)
    end
    @test_throws ArgumentError poly("x + z")
    @test occursin("not polynomial", sprint(showerror, NotPolynomial("why")))
    goal = read_goal("""{"schema": "maithic.oracle.goal/1", "kind": "polynomial",
                         "hypotheses": ["x + y = 3", "x * y = 2"], "goal": "x^2 + y^2 = 5"}""")
    @test goal.kind == :polynomial && goal.domain == :rat && goal.variables == XY
    @test prove_polynomial(goal) isa Proved
    @test degree(poly("x^2 * y + x")) == 3 && degree(poly("7")) == 0 && degree(poly("0")) == -1
    @test evaluate_polynomial(poly("x^2 * y - 3 * y + 1 / 2"), [2, 5]) == 20 - 15 + 1 // 2
end

@testset "P13-03-02 every polynomial has one normal form" begin
    monomials = [[0, 0, 0], [0, 0, 1], [0, 1, 0], [1, 0, 0], [0, 0, 2], [0, 1, 1], [1, 0, 1], [0, 2, 0], [1, 1, 0], [2, 0, 0]]
    @test issorted(monomials; lt = monomial_less) && allunique(monomials)
    @test !monomial_less([1, 1, 0], [1, 1, 0])
    @test polynomial_text(poly("(x + y)^2 - x * y / 2 - 3 / 4"), XY) == "x^2 + 3 / 2 * x * y + y^2 - 3 / 4"
    @test polynomial_text(poly("-x * y + 2"), XY) == "-x * y + 2"
    @test polynomial_text(poly("-2 * x^2 * y - y"), XY) == "-2 * x^2 * y - y"
    @test polynomial_text(poly("x - x"), XY) == "0"
    @test polynomial_text(poly("z + y + x + 1", XYZ), XYZ) == "x + y + z + 1"
    @test polynomial_text(poly("x^2 - 1"), XY; style = :lean) == "x ^ 2 - 1"
    @test poly("(x + y) * (x - y)") == poly("x^2 - y^2")
    @test poly("(x + y)^3") == poly("x^3 + 3 * x^2 * y + 3 * x * y^2 + y^3")
    @test poly("(x + y)^3") != poly("x^3 + y^3")
    @test hash(poly("(x + y) * (x - y)")) == hash(poly("x^2 - y^2"))
    @test_throws ArgumentError poly("x")^(1 - 2)
    rng = SplitMix(13)
    for _ in 1:200
        p, q, r = (random_polynomial(rng, 3, integer!(rng, 0:4), 3) for _ in 1:3)
        @test polynomial(parse_expression(polynomial_text(p, XYZ)), XYZ) == p
        @test (p + q) * r == p * r + q * r
        @test p - p == zero(p) && p + q == q + p && p * q == q * p
        @test issorted(p.terms; lt = (s, t) -> monomial_less(t[1], s[1])) && all(!iszero(c) for (_, c) in p.terms)
        point = [integer!(rng, -3:3) for _ in 1:3]
        @test evaluate_polynomial(p * q - r, point) ==
              evaluate_polynomial(p, point) * evaluate_polynomial(q, point) - evaluate_polynomial(r, point)
        @test evaluate(polynomial_node(p, XYZ), Dict(zip(XYZ, point))) == evaluate_polynomial(p, point)
    end
end

@testset "P13-03-03 identities are decided exactly" begin
    outcome = prove_polynomial(String[], "(x + y)^2 = x^2 + 2*x*y + y^2")
    @test outcome isa Proved && kind(outcome.certificate) == "polynomial.identity" && check(outcome.certificate)
    @test claim(outcome.certificate) == "∀ x y : ℚ, (x + y) ^ 2 = x ^ 2 + 2 * x * y + y ^ 2"
    @test lean_check(outcome.certificate) ==
          "example (x y : Rat) : ((x + y) ^ 2 : Rat) = x ^ 2 + 2 * x * y + y ^ 2 := by grind\n"
    four_squares = prove_polynomial(String[], "(a^2 + b^2) * (c^2 + d^2) = (a*c - b*d)^2 + (a*d + b*c)^2")
    @test four_squares isa Proved
    @test prove_polynomial(String[], "(x - 1/2)^2 = x^2 - x + 0.25") isa Proved
    @test prove_polynomial(String[], "2 + 2 = 4") isa Proved
    freshman = prove_polynomial(String[], "(x + y)^2 = x^2 + y^2")
    @test freshman == Refuted(["x" => Exact(1), "y" => Exact(1)])
    @test prove_polynomial(String[], "2 + 2 = 5") == Refuted(Pair{String,Exact}[])
    @test prove_polynomial(String[], "x * (x - 1) * (x - 2) = 0") == Refuted(["x" => Exact(3)])
    @test nonroot(poly("x * y * (x - 1) * (y - 1)")) == [2, 2]
    @test prove_polynomial(String[], "x^2 >= 0") == Unknown("the polynomial oracle proves equations, and the goal is not one")
    @test prove_polynomial(String[], "false") isa Unknown
    @test prove_polynomial(String[], "sqrt(x)^2 = x") == Unknown("the goal is not polynomial: the function sqrt is applied")
end

@testset "P13-03-04 goals in the ideal come with cofactors" begin
    outcome = prove_polynomial(["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5")
    c = outcome.certificate
    @test kind(c) == "polynomial.combination" && check(c)
    @test [polynomial_text(q, XY) for q in c.cofactors] == ["x + y + 3", "-2"]
    @test claim(c) == "∀ x y : ℚ, x + y = 3 → x * y = 2 → x ^ 2 + y ^ 2 = 5"
    @test lean_check(c) ==
          "example (x y : Rat) : (x ^ 2 + y ^ 2 - 5 : Rat) = (x + y + 3) * (x + y - 3) + -2 * (x * y - 2) := by grind\n"
    cubes = prove_polynomial(["a = b", "b = c"], "a^3 = c^3").certificate
    @test check(cubes) && length(cubes.cofactors) == 2
    @test check(prove_polynomial(["x^2 = 1"], "x^4 = 1").certificate)
    @test check(prove_polynomial(["x^2 + y^2 = 1", "x = y"], "2 * y^2 = 1").certificate)
    fractions = prove_polynomial(["x/2 + y/3 = 1", "x = y"], "5 * x = 6").certificate
    @test check(fractions)
    unused = prove_polynomial(["x = 1", "y = 7"], "x^2 = 1").certificate
    @test check(unused) && iszero(unused.cofactors[2])
    trivial = prove_polynomial(["x = 1"], "y = y").certificate
    @test check(trivial) && all(iszero, trivial.cofactors)
    integers = prove_polynomial(["2*x = 3*y + 1", "y = 1"], "x = 2"; domain = :int).certificate
    @test check(integers) && claim(integers) == "∀ x y : ℤ, 2 * x = 3 * y + 1 → y = 1 → x = 2"
    @test [polynomial_text(q, XY) for q in integers.cofactors] == ["1 / 2", "3 / 2"]
    @test lean_check(integers) ==
          "example (x y : Int) : (2 * (x - 2) : Int) = 1 * (2 * x - (3 * y + 1)) + 3 * (y - 1) := by grind\n"
    @test lean_check(prove_polynomial(["x / 2 = 1"], "x = 2"; domain = :int).certificate) === nothing
    @test lean_check(prove_polynomial(["x = 1"], "x^2 = 1"; domain = :real).certificate) === nothing
    @test claim(prove_polynomial(["x = 1"], "x^2 = 1"; domain = :real).certificate) == "∀ x : ℝ, x = 1 → x ^ 2 = 1"
    # In the radical but not in the ideal: true, and beyond this method.
    @test prove_polynomial(["x^2 = 0"], "x = 0") == Unknown("the goal is not a polynomial combination of the hypotheses")
    @test prove_polynomial(["x + y = 3"], "x = 1") isa Unknown
    @test prove_polynomial(["x >= 0"], "x^2 = x^2") == Unknown("hypothesis h1 is not an equation")
    @test prove_polynomial(["exp(x) = 1"], "x = 0") isa Unknown
end

@testset "P13-03-05 Groebner bases by Buchberger's algorithm" begin
    circle = groebner([poly("x^2 + y^2 - 1"), poly("x - y")])
    @test is_groebner(circle) && minimal_leading(circle) == [[0, 2], [1, 0]]
    hyperbola = groebner([poly("x * y - 1"), poly("y^2 - 1")])
    @test is_groebner(hyperbola) && minimal_leading(hyperbola) == [[0, 2], [1, 0]]
    cyclic = groebner([poly("x + y + z", XYZ), poly("x*y + y*z + z*x", XYZ), poly("x*y*z - 1", XYZ)])
    @test is_groebner(cyclic) && minimal_leading(cyclic) == [[0, 0, 3], [0, 2, 0], [1, 0, 0]]
    generators = [poly("x^2 * y - 1"), poly("x * y^2 - x")]
    basis = groebner(generators)
    @test is_groebner(basis)
    for element in basis                      # every element is the stated combination of the generators
        @test sum(q * g for (q, g) in zip(element.cofactors, generators)) == element.polynomial
        @test isone(MaithicOracle.leading_coefficient(element.polynomial))
    end
    @test all(iszero(divide(g, [b.polynomial for b in basis])[2]) for g in generators)
    @test groebner(Polynomial[]) == Tracked[]
    @test length(groebner([poly("0"), poly("x - 1")])) == 1
    @test minimal_leading(groebner([poly("x - 1"), poly("x - 2")])) == [[0, 0]]     # the whole ring: 1 is in the ideal
    quotients, remainder = divide(poly("x^2 * y + x * y^2 + y^2"), [poly("x * y - 1"), poly("y^2 - 1")])
    @test (polynomial_text.(quotients, Ref(XY)), polynomial_text(remainder, XY)) == (["x + y", "1"], "x + y + 1")
    @test membership(poly("x^2 + y^2 - 5"), [poly("x + y - 3"), poly("x * y - 2")]) == [poly("x + y + 3"), poly("-2")]
    @test membership(poly("x"), [poly("x^2")]) === nothing
    @test membership(poly("0"), [poly("x^2")]) == [poly("0")]
    @test membership(poly("x - 1"), Polynomial[]) === nothing
    @test groebner([poly("x^2 * y - 1"), poly("x * y^2 - x")]; budget = Budget(3)) === nothing
    @test groebner([poly("x^2 * y - 1"), poly("x * y^2 - x")]; max_basis = 2) === nothing
    @test membership(poly("x^3 - y"), [poly("x^2 * y - 1"), poly("x * y^2 - x")]; budget = Budget(3)) === :budget
    @test prove_polynomial(["x^2 * y = 1", "x * y^2 = x"], "y^2 = 1"; steps = 3) ==
          Unknown("the Gröbner basis computation exceeded its budget of 3 steps")
    @test prove_polynomial(["x^2 * y = 1", "x * y^2 = x"], "y^2 = 1") isa Proved
end

@testset "P13-03-06 polynomial certificates are written in the shared schema" begin
    c = prove_polynomial(["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5").certificate
    @test certificate_json(c) ==
          "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"polynomial.combination\"," *
          "\"claim\":\"∀ x y : ℚ, x + y = 3 → x * y = 2 → x ^ 2 + y ^ 2 = 5\"," *
          "\"data\":{\"domain\":\"rat\",\"variables\":[\"x\",\"y\"],\"hypotheses\":[" *
          "{\"name\":\"h1\",\"statement\":\"x + y = 3\"},{\"name\":\"h2\",\"statement\":\"x * y = 2\"}]," *
          "\"goal\":\"x^2 + y^2 = 5\",\"cofactors\":[\"x + y + 3\",\"-2\"]}," *
          "\"engine\":{\"name\":\"MaithicOracle\",\"version\":\"0.1.0\",\"parameters\":{}}}"
    for certificate in (c, prove_polynomial(String[], "(x + y)^2 = x^2 + 2*x*y + y^2").certificate,
                        prove_polynomial(["2*x = 3*y + 1", "y = 1"], "x = 2"; domain = :int).certificate,
                        prove_polynomial(["x/2 + y/3 = 1", "x = y"], "5 * x = 6").certificate,
                        prove_polynomial(["a = b", "b = c"], "a^3 = c^3").certificate)
        @test round_trip(certificate)
    end
    @test_throws ArgumentError read_certificate(replace(certificate_json(c), "\"-2\"" => "\"sqrt(x)\""))
    @test_throws ArgumentError read_certificate(replace(certificate_json(c), "\"-2\"" => "\"z\""))
end

@testset "P13-03-07 the combination must expand to the goal" begin
    good = prove_polynomial(["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5").certificate
    tampered(; domain = good.domain, variables = good.variables, hypotheses = good.hypotheses, goal = good.goal,
             cofactors = good.cofactors) = PolynomialCertificate(domain, variables, hypotheses, goal, cofactors)
    wrong = [tampered(cofactors = [poly("x + y + 3"), poly("-1")]),
             tampered(cofactors = [poly("x + y + 3")]),
             tampered(cofactors = reverse(good.cofactors)),
             tampered(goal = parse_relation("x^2 + y^2 = 6")),
             tampered(goal = parse_relation("x^2 + y^2 <= 5")),
             tampered(hypotheses = [good.hypotheses[1], Hypothesis("h2", parse_relation("x * y >= 2"))]),
             tampered(hypotheses = [good.hypotheses[1], Hypothesis("h1", parse_relation("x * y = 2"))]),
             tampered(hypotheses = [good.hypotheses[1], Hypothesis("h2", parse_relation("x * y = 3"))]),
             tampered(variables = ["x", "y", "z"]),
             tampered(domain = :complex),
             tampered(goal = parse_relation("sqrt(x) = 5")),
             PolynomialCertificate(:rat, XY, Hypothesis[], parse_relation("(x + y)^2 = x^2 + y^2"), Polynomial[])]
    for c in wrong
        @test !check(c)
        @test_throws RejectedCertificate certificate_document(c)
    end
    @test check(good)
    forged = replace(certificate_json(good), "\"-2\"" => "\"-3\"")
    @test !check(read_certificate(forged))
end

@testset "P13-03-08 generated identities and combinations" begin
    rng = SplitMix(404)
    text(p) = polynomial_text(p, XYZ)
    for _ in 1:120
        a, b, c = (random_polynomial(rng, 3, integer!(rng, 1:3), 2) for _ in 1:3)
        identity = prove_polynomial(String[], "($(text(a)) + $(text(b))) * ($(text(c))) = $(text(a * c + b * c))"; variables = XYZ)
        @test identity isa Proved && round_trip(identity.certificate)
        bump = random_polynomial(rng, 3, 1, 2)
        iszero(bump) && continue
        goal = Goal(:polynomial, String[], "($(text(a))) * ($(text(b))) = $(text(a * b + bump))"; variables = XYZ)
        outcome = prove_polynomial(goal)
        @test outcome isa Refuted && MaithicOracle.is_counterexample(goal, outcome.model)
    end
    proved = 0
    for _ in 1:80
        generators = [random_polynomial(rng, 2, integer!(rng, 1:3), 2) for _ in 1:2]
        any(iszero, generators) && continue
        multipliers = [random_polynomial(rng, 2, integer!(rng, 1:2), 1) for _ in 1:2]
        target = sum(q * g for (q, g) in zip(multipliers, generators))
        outcome = prove_polynomial(["$(polynomial_text(g, XY)) = 0" for g in generators], "$(polynomial_text(target, XY)) = 0";
                                   variables = XY, steps = 20_000)
        outcome isa Unknown && continue            # only the step budget can stop a goal that is in the ideal
        proved += 1
        @test outcome isa Proved && check(outcome.certificate) && round_trip(outcome.certificate)
    end
    @test proved > 60
end
