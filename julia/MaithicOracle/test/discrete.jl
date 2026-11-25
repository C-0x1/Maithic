# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

const MERSENNE_61 = big(2)^61 - 1
const MERSENNE_89 = big(2)^89 - 1

@testset "P13-06-01 primality has Pratt certificates" begin
    c = prove_prime(97)
    @test c isa PrimeCertificate && check(c)
    @test last(c.proof) == PrattEntry(97, 5, [(big(2), 5), (big(3), 1)])
    @test claim(c) == "Nat.Prime 97"
    @test kind(c) == "discrete.prime"
    @test check(prove_prime(2)) && isempty(prove_prime(2).proof)
    @test check(prove_prime(3))
    for composite in (-7, 0, 1, 4, 91, 561, 1105, 6601, 3215031751, big(2)^61 + 1, 1000001970000133)
        @test prove_prime(composite) === nothing
    end
    primes_below_600 = [n for n in 1:600 if prove_prime(n) !== nothing]
    @test primes_below_600 == [n for n in 2:600 if all(n % d != 0 for d in 2:isqrt(n))]
    @test all(check(prove_prime(p)) for p in primes_below_600)
    big_one = prove_prime(MERSENNE_61)
    @test check(big_one) && last(big_one.proof).n == MERSENNE_61
    @test check(prove_prime(MERSENNE_89))
    @test check(prove_prime(big(10)^24 + 7))
    @test is_probable_prime(MERSENNE_89) && !is_probable_prime(MERSENNE_89 + 2) && !is_probable_prime(1)
end

@testset "P13-06-02 factorizations are proved prime by prime" begin
    c = factorization(360)
    @test c.factors == [(big(2), 3), (big(3), 2), (big(5), 1)]
    @test check(c) && claim(c) == "360 = 2^3 * 3^2 * 5"
    @test lean_check(c) == "example : (360 : Nat) = 2 ^ 3 * 3 ^ 2 * 5 ^ 1 := by decide\n"
    @test lean_check(c; name = "three_sixty") == "theorem three_sixty : (360 : Nat) = 2 ^ 3 * 3 ^ 2 * 5 ^ 1 := by decide\n"
    @test lean_check(prove_prime(97); name = "p") === nothing
    @test_throws ArgumentError lean_check(c; name = "three sixty")
    @test check(factorization(1)) && isempty(factorization(1).factors)
    @test factorization(97).factors == [(big(97), 1)]
    semiprime = factorization(big(1000003) * big(999983))
    @test semiprime.factors == [(big(999983), 1), (big(1000003), 1)] && check(semiprime)
    @test check(factorization(big(2)^64 + 1))
    @test factorization(big(2)^64 + 1).factors == [(big(274177), 1), (big(67280421310721), 1)]
    @test check(factorization(factorial(big(25))))
    @test prime_factors(big(1000003)^2) == [(big(1000003), 2)]
    @test_throws DomainError prime_factors(0)
    @test_throws FactoringBudget prime_factors(big(1000003) * big(999983); budget = 1)
    rng = SplitMix(9)
    for _ in 1:150
        n = integer!(rng, 1:10^12)
        f = factorization(n)
        @test check(f) && prod((p^e for (p, e) in f.factors); init = big(1)) == n
    end
end

@testset "P13-06-03 modular facts carry their witnesses" begin
    inverse = modular_inverse(3, 7)
    @test inverse.inverse == 5 && check(inverse)
    @test claim(inverse) == "3 * 5 ≡ 1 [ZMOD 7]"
    @test lean_check(inverse) == "example : (3 * 5 : Int) % 7 = 1 := by decide\n"
    @test modular_inverse(-3, 7).inverse == 2 && check(modular_inverse(-3, 7))
    @test lean_check(modular_inverse(-3, 7)) == "example : ((-3) * 2 : Int) % 7 = 1 := by decide\n"
    @test modular_inverse(6, 9) === nothing
    @test_throws DomainError modular_inverse(1, 1)

    order = multiplicative_order(3, 7)
    @test order.order == 6 && check(order) && claim(order) == "orderOf (3 : ZMod 7) = 6"
    @test multiplicative_order(2, 7).order == 3
    @test multiplicative_order(1, 5).order == 1 && check(multiplicative_order(1, 5))
    @test multiplicative_order(10, 1_000_003).order == 1_000_002 ÷ 6
    @test multiplicative_order(6, 9) === nothing
    @test check(multiplicative_order(2, MERSENNE_61)) && multiplicative_order(2, MERSENNE_61).order == 61
    for m in 2:60, a in 1:(m - 1)
        gcd(a, m) == 1 || continue
        expected = findfirst(k -> powermod(a, k, m) == 1, 1:m)
        @test multiplicative_order(a, m).order == expected
    end

    log = discrete_logarithm(3, 13, 17)
    @test log.exponent == 4 && check(log) && claim(log) == "3 ^ 4 ≡ 13 [ZMOD 17]"
    @test discrete_logarithm(2, 1, 7).exponent == 0
    @test discrete_logarithm(2, 3, 7) === nothing
    @test discrete_logarithm(2, 0, 1024).exponent == 10
    @test discrete_logarithm(2, big(2)^150, big(2)^200).exponent == 150
    @test discrete_logarithm(6, 0, 72).exponent == 3
    @test discrete_logarithm(5, powermod(5, 123456, 1_000_003), 1_000_003).exponent == 123456
    @test discrete_logarithm(5, powermod(5, 123456, 1_000_003), 1_000_003; limit = 1000) === nothing
    for m in 2:40, g in 0:(m - 1), h in 0:(m - 1)
        expected = findfirst(x -> powermod(g, x, m) == h, 0:(2m))
        found = discrete_logarithm(g, h, m)
        @test (found === nothing ? nothing : found.exponent + 1) == expected
    end

    root = modular_square_root(2, 7)
    @test root.root == 3 && check(root) && claim(root) == "3 ^ 2 ≡ 2 [ZMOD 7]"
    @test lean_check(root) == "example : ((3 : Int) ^ 2 - 2) % 7 = 0 := by decide\n"
    @test modular_square_root(3, 7) === nothing
    @test modular_square_root(0, 7).root == 0
    @test check(modular_square_root(5, MERSENNE_61))
    @test modular_square_root(17, 2^10).root^2 % 2^10 == 17
    @test modular_square_root(3, 2^10) === nothing
    @test modular_square_root(1, 2).root == 1
    @test modular_square_root(4, big(10)^30) === nothing    # too large to search, and not prime: no answer
    for p in (3, 5, 13, 17, 97, 257, 65537, 1_000_003), a in 0:12
        found = modular_square_root(a, p)
        @test (found !== nothing) == (a % p == 0 || powermod(a, (p - 1) ÷ 2, p) == 1)
        found === nothing || @test check(found)
    end
end

@testset "P13-06-04 Bezout coefficients prove a greatest common divisor" begin
    c = bezout(12, 18)
    @test (c.g, c.x, c.y, c.a_cofactor, c.b_cofactor) == (6, -1, 1, 2, 3)
    @test check(c) && claim(c) == "Int.gcd 12 18 = 6"
    @test lean_check(c) ==
          "example : (12 : Int) * (-1) + 18 * 1 = 6 ∧ (6 : Int) * 2 = 12 ∧ (6 : Int) * 3 = 18 := by decide\n"
    @test claim(bezout(-4, 6)) == "Int.gcd (-4) 6 = 2"
    @test bezout(0, 0).g == 0 && check(bezout(0, 0))
    @test bezout(0, -5).g == 5 && check(bezout(0, -5))
    @test bezout(big(2)^200 - 1, big(2)^120 - 1).g == big(2)^40 - 1
    rng = SplitMix(4)
    for _ in 1:500
        a, b = integer!(rng, -10^9:10^9), integer!(rng, -10^9:10^9)
        c = bezout(a, b)
        @test check(c) && c.g == gcd(a, b)
    end
end

@testset "P13-06-05 existential goals get explicit witnesses" begin
    c = find_witness("x^2 + y^2 = 25 ∧ x > y ∧ y > 0")
    @test c.variables == ["x", "y"] && c.values == [4, 3] && check(c)
    @test claim(c) == "∃ x y : ℤ, x ^ 2 + y ^ 2 = 25 ∧ x > y ∧ y > 0"
    @test lean_check(c) == "example : ∃ x y : Int, x ^ 2 + y ^ 2 = 25 ∧ x > y ∧ y > 0 := ⟨4, 3, by decide⟩\n"
    negative = find_witness("x * x = 49 and x < 0")
    @test negative.values == [-7]
    @test lean_check(negative) == "example : ∃ x : Int, x * x = 49 ∧ x < 0 := ⟨-7, by decide⟩\n"
    @test find_witness("x * x = 49 and x < 0"; domain = :nat) === nothing
    pell = find_witness("x^2 = 2*y^2 + 1 ∧ y > 0"; domain = :nat)
    @test pell.values == [3, 2] && claim(pell) == "∃ x y : ℕ, x ^ 2 = 2 * y ^ 2 + 1 ∧ y > 0"
    @test find_witness("x^2 = 2") === nothing
    @test find_witness("x > 1000") === nothing
    @test find_witness("x > 1000"; bound = 2000).values == [1001]
    @test find_witness("x + y + z = 3 ∧ x * y * z = -8 ∧ x <= y ∧ y <= z"; bound = 8).values == [-2, 1, 4]
    @test find_witness("x = y"; variables = ["y", "x", "unused"]).values == [0, 0, 0]
    @test find_witness("1 / x = 1").values == [1]          # the point x = 0 is skipped, not an error
    @test lean_check(find_witness("1 / x = 1")) === nothing # division means something else in Lean
    @test lean_check(find_witness("x - 1 = 0"; domain = :nat)) === nothing
    @test find_witness("x^3 + y^3 = 1729 ∧ 0 < x ∧ x < y"; budget = 10) === nothing
    @test find_witness("x^3 + y^3 = 1729 ∧ 0 < x ∧ x < y").values == [9, 10]    # smallest in the largest coordinate
    @test find_witness("x^3 + y^3 = 1729 ∧ 0 < x ∧ x < 9 ∧ x < y").values == [1, 12]
    @test_throws ArgumentError find_witness("x = y"; variables = ["x"])
    @test_throws ArgumentError find_witness("x = 1"; domain = :rat)
    @test_throws ArgumentError find_witness(Relation[])
    shell = Vector{Int}[]
    MaithicOracle.visit_shell(p -> (push!(shell, copy(p)); false), zeros(Int, 2), 2, false)
    @test length(shell) == 16 && allunique(shell) && all(maximum(abs, p) == 2 for p in shell) && issorted(shell)
    counts = [begin
                  n = Ref(0)
                  MaithicOracle.visit_shell(_ -> (n[] += 1; false), zeros(Int, 3), r, true)
                  n[]
              end for r in 0:4]
    @test counts == [(r + 1)^3 - r^3 for r in 0:4]
end

@testset "P13-06-06 certificates are written in the shared schema" begin
    certificates = [prove_prime(97), factorization(360), modular_inverse(3, 7), multiplicative_order(3, 7),
                    discrete_logarithm(3, 13, 17), modular_square_root(2, 7), bezout(12, 18),
                    find_witness("x^2 + y^2 = 25 ∧ x > y ∧ y > 0")]
    @test allunique(kind(c) for c in certificates)
    @test issubset([kind(c) for c in certificates], MaithicOracle.certificate_kinds())
    for c in certificates
        doc = certificate_document(c, "seed" => 1)
        @test keys(doc) == ["schema", "kind", "claim", "data", "engine"]
        @test doc["schema"] == "maithic.oracle.certificate/1" && doc["kind"] == kind(c) && doc["claim"] == claim(c)
        @test doc["engine"]["name"] == "MaithicOracle" && doc["engine"]["parameters"]["seed"] == 1
        @test round_trip(c)
        @test !occursin(r"[0-9]\.[0-9]", write_json(doc["data"]))    # exact text only, never a float
    end
    @test certificate_json(bezout(12, 18)) ==
          "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.bezout\",\"claim\":\"Int.gcd 12 18 = 6\"," *
          "\"data\":{\"a\":\"12\",\"b\":\"18\",\"gcd\":\"6\",\"x\":\"-1\",\"y\":\"1\",\"a_cofactor\":\"2\"," *
          "\"b_cofactor\":\"3\"},\"engine\":{\"name\":\"MaithicOracle\",\"version\":\"0.1.0\",\"parameters\":{}}}"
    big_numbers = certificate_document(factorization(big(2)^64 + 1))["data"]
    @test big_numbers["n"] == "18446744073709551617" && big_numbers["factors"][2][1] == "67280421310721"
    for bad in ("{}", "{\"schema\":\"maithic.oracle.certificate/1\"}",
                "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.unheard\",\"data\":{}}",
                "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.bezout\"}",
                "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.bezout\",\"data\":{\"a\":\"1\"}}",
                "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.bezout\",\"data\":{\"a\":12,\"b\":\"18\"," *
                "\"gcd\":\"6\",\"x\":\"-1\",\"y\":\"1\",\"a_cofactor\":\"2\",\"b_cofactor\":\"3\"}}",
                "{\"schema\":\"maithic.oracle.certificate/1\",\"kind\":\"discrete.witness\",\"data\":{\"domain\":\"int\"," *
                "\"variables\":[\"x\"],\"conditions\":[\"x +\"],\"values\":[\"1\"]}}")
        @test_throws ArgumentError read_certificate(bad)
    end
end

@testset "P13-06-07 a wrong witness is caught before it is written" begin
    wrong = [PrimeCertificate(91, [PrattEntry(91, 2, [(big(2), 1), (big(3), 2), (big(5), 1)])]),
             PrimeCertificate(97, PrattEntry[]),
             PrimeCertificate(97, [PrattEntry(97, 2, [(big(2), 5), (big(3), 1)])]),          # 2 is not a generator
             PrimeCertificate(97, [PrattEntry(97, 5, [(big(2), 4), (big(6), 1)])]),          # 6 is not prime
             PrimeCertificate(97, [PrattEntry(97, 5, [(big(2), 5)])]),                        # 3 left out
             PrimeCertificate(23, [PrattEntry(23, 5, [(big(2), 1), (big(11), 1)])]),          # 11 not proved
             FactorizationCertificate(360, [(big(2), 3), (big(3), 2), (big(5), 2)], prove_prime(5).proof),
             FactorizationCertificate(360, [(big(2), 3), (big(45), 1)], PrattEntry[]),
             FactorizationCertificate(360, [(big(5), 1), (big(3), 2), (big(2), 3)], factorization(360).proof),
             FactorizationCertificate(12, [(big(2), 1), (big(2), 1), (big(3), 1)], factorization(12).proof),
             InverseCertificate(3, 7, 4), InverseCertificate(3, 7, 12), InverseCertificate(1, 1, 0),
             OrderCertificate(2, 7, 6, [(big(2), 1), (big(3), 1)], prove_prime(3).proof),      # the order is 3
             OrderCertificate(3, 7, 12, [(big(2), 2), (big(3), 1)], prove_prime(3).proof),
             OrderCertificate(3, 7, 6, [(big(2), 1)], PrattEntry[]),
             LogarithmCertificate(3, 13, 17, 5), LogarithmCertificate(3, 13, 17, -1),
             SquareRootCertificate(2, 7, 2), SquareRootCertificate(2, 7, 10),
             BezoutCertificate(12, 18, 6, 1, 1, 2, 3), BezoutCertificate(12, 18, 3, -2, 1, 4, 6) ,
             BezoutCertificate(12, 18, 6, -1, 1, 2, 4), BezoutCertificate(-12, -18, -6, -1, 2, 2, 3),
             WitnessCertificate(:int, ["x", "y"], parse_conjunction("x^2 + y^2 = 25"), BigInt[4, 4]),
             WitnessCertificate(:nat, ["x"], parse_conjunction("x * x = 49"), BigInt[-7]),
             WitnessCertificate(:int, ["x"], parse_conjunction("x = y"), BigInt[1]),
             WitnessCertificate(:int, ["x"], parse_conjunction("1 / x = 1"), BigInt[0]),
             WitnessCertificate(:int, ["x"], parse_conjunction("sqrt(x) = 2"), BigInt[4]),
             WitnessCertificate(:rat, ["x"], parse_conjunction("x = 1"), BigInt[1]),
             WitnessCertificate(:int, ["x", "x"], parse_conjunction("x = 1"), BigInt[1, 1])]
    for c in wrong
        @test !check(c)
        @test_throws RejectedCertificate certificate_document(c)
    end
    @test occursin("discrete.prime", sprint(showerror, RejectedCertificate("discrete.prime")))
    # A certificate that is well formed but false is read, and then fails its check.
    forged = replace(certificate_json(bezout(12, 18)), "\"gcd\":\"6\"" => "\"gcd\":\"3\"")
    @test !check(read_certificate(forged))
end

@testset "P13-06-08 every kind of witness holds on random questions" begin
    rng = SplitMix(2026)
    for _ in 1:120
        m = integer!(rng, 2:5000)
        a = integer!(rng, 0:(10 * m))
        inverse = modular_inverse(a, m)
        @test (inverse === nothing) == (gcd(a, m) != 1)
        inverse === nothing || @test round_trip(inverse)
        order = multiplicative_order(a, m)
        @test (order === nothing) == (gcd(a, m) != 1)
        if order !== nothing
            @test round_trip(order)
            @test all(powermod(a, k, m) != 1 for k in 1:min(Int(order.order) - 1, 300))
        end
        x = integer!(rng, 0:200)
        log = discrete_logarithm(a, powermod(a, x, m), m)
        @test log !== nothing && log.exponent <= x && round_trip(log)
        r = integer!(rng, 0:(m - 1))
        root = modular_square_root(r * r, m)
        @test root !== nothing && round_trip(root)
    end
    for _ in 1:60
        n = integer!(rng, 2:10^9)
        prime = prove_prime(n)
        @test (prime !== nothing) == is_probable_prime(n)
        prime === nothing || @test round_trip(prime)
        @test round_trip(factorization(n))
        a, b, c = integer!(rng, -30:30), integer!(rng, -30:30), integer!(rng, -30:30)
        witness = find_witness(Relation[Relation(:eq, Var("x") * a + Var("y") * b, Num(c))]; bound = 40)
        solvable = (a == 0 && b == 0) ? c == 0 : c % gcd(a, b) == 0
        @test (witness !== nothing) == solvable
        witness === nothing || @test round_trip(witness)
    end
end
