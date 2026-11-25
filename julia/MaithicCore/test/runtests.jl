# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

using Test
using MaithicCore

const X = Var("x")
const Y = Var("y")

"A random expression over x and y, built from every kind of node."
function random_node(rng::SplitMix, depth::Int)
    choice = depth == 0 ? integer!(rng, 1:2) : integer!(rng, 1:8)
    if choice == 1
        Num(integer!(rng, -9:9) // integer!(rng, 1:4))
    elseif choice == 2
        integer!(rng, 0:1) == 0 ? X : Y
    elseif choice == 3
        Neg(random_node(rng, depth - 1))
    elseif choice == 4
        Pow(random_node(rng, depth - 1), integer!(rng, 0:3))
    elseif choice == 5
        Call(integer!(rng, 0:1) == 0 ? :min : :max, Node[random_node(rng, depth - 1), random_node(rng, depth - 1)])
    elseif choice == 6
        Call(:abs, Node[random_node(rng, depth - 1)])
    else
        BinOp((:+, :-, :*)[integer!(rng, 1:3)], random_node(rng, depth - 1), random_node(rng, depth - 1))
    end
end

@testset "MaithicCore" begin
    @testset "JSON values are written deterministically" begin
        @test write_json(nothing) == "null"
        @test write_json(true) == "true"
        @test write_json(-12) == "-12"
        @test write_json(big(2)^100) == "1267650600228229401496703205376"
        @test write_json(0.1) == "0.1"
        @test write_json(1.0e-5) == "1.0e-5"
        @test write_json("a\"b\\c\n\t\u0001é") == "\"a\\\"b\\\\c\\n\\t\\u0001é\""
        @test write_json([1, "two", nothing, [3.5]]) == "[1,\"two\",null,[3.5]]"
        @test write_json((1, :two)) == "[1,\"two\"]"
        @test write_json(Object("b" => 1, "a" => 2)) == "{\"b\":1,\"a\":2}"
        @test write_json(Object("b" => 1, "a" => 2); canonical = true) == "{\"a\":2,\"b\":1}"
        @test write_json(Dict("b" => 1, "a" => 2)) == "{\"a\":2,\"b\":1}"
        @test write_json((schema = "s", n = 1)) == "{\"schema\":\"s\",\"n\":1}"
        @test write_json(Object()) == "{}"
        @test write_json(Any[]) == "[]"
        @test_throws ArgumentError write_json(NaN)
        @test_throws ArgumentError write_json(Inf)
        @test_throws ArgumentError write_json(1 // 2)
        @test_throws ArgumentError write_json(1 + 2im)
    end

    @testset "pretty JSON keeps plain arrays on one line" begin
        value = Object("name" => "m", "sizes" => [1, 2, 3], "nested" => Object("items" => Any[Object("a" => 1), Any[]]))
        text = write_json(value; pretty = true)
        @test text == """
        {
          "name": "m",
          "sizes": [1, 2, 3],
          "nested": {
            "items": [
              {
                "a": 1
              },
              []
            ]
          }
        }"""
        @test read_json(text) == value
    end

    @testset "JSON is read strictly" begin
        @test read_json(" [1, 2.5, -3e2, \"x\", true, false, null] ") == Any[1, 2.5, -300.0, "x", true, false, nothing]
        @test read_json("12345678901234567890123") == parse(BigInt, "12345678901234567890123")
        @test read_json("9223372036854775807") isa Int64
        @test read_json("\"\\u00e9\\ud83d\\ude00\\/\\b\\f\"") == "é😀/\b\f"
        object = read_json("{\"z\": 1, \"a\": {\"k\": []}}")
        @test keys(object) == ["z", "a"]
        @test object["a"]["k"] == Any[]
        @test get(object, "missing", 7) == 7
        @test_throws KeyError object["missing"]
        for bad in ("", "[1,]", "{\"a\":1,}", "{\"a\":1 \"b\":2}", "{a:1}", "[1] 2", "01", "1.", "-", "\"\\x\"",
                    "\"a\nb\"", "\"\\ud83d\"", "\"\\ude00\"", "tru", "{\"a\":1,\"a\":2}", "[1", "\"open", "+1", ".5",
                    "1e", "nul", "[1 2]", "\"\\u12g4\"")
            @test_throws JSONError read_json(bad)
        end
        @test_throws JSONError read_json("["^300 * "]"^300)
        @test read_json("["^200 * "]"^200) isa Vector
        error = try
            read_json("[1, x]")
        catch e
            e
        end
        @test error.offset == 5
        @test occursin("byte 5", sprint(showerror, error))
    end

    @testset "JSON round trips" begin
        rng = SplitMix(11)
        function random_value(depth)
            choice = depth == 0 ? integer!(rng, 1:5) : integer!(rng, 1:7)
            choice == 1 ? nothing :
            choice == 2 ? (integer!(rng, 0:1) == 1) :
            choice == 3 ? integer!(rng, -1000:1000) :
            choice == 4 ? uniform!(rng, -1.0e6, 1.0e6) :
            choice == 5 ? join(rand_char() for _ in 1:integer!(rng, 0:6)) :
            choice == 6 ? Any[random_value(depth - 1) for _ in 1:integer!(rng, 0:3)] :
            Object(("k$(i)" => random_value(depth - 1) for i in 1:integer!(rng, 0:3))...)
        end
        rand_char() = ('a', 'Z', '"', '\\', '\n', 'é', '😀', ' ', '\u0007')[integer!(rng, 1:9)]
        for _ in 1:300
            value = random_value(3)
            @test read_json(write_json(value)) == value
            @test read_json(write_json(value; pretty = true)) == value
            @test write_json(read_json(write_json(value))) == write_json(value)
        end
    end

    @testset "objects" begin
        object = Object("a" => 1)
        object["b"] = 2
        object["a"] = 3
        @test collect(object) == ["a" => 3, "b" => 2]
        @test length(object) == 2 && !isempty(object) && haskey(object, "b") && !haskey(object, "c")
        @test Object("a" => 1, "b" => 2) == Object("b" => 2, "a" => 1)
        @test Object("a" => 1) != Object("a" => 2)
        @test hash(Object("a" => 1, "b" => 2)) == hash(Object("b" => 2, "a" => 1))
        @test sprint(show, Object(:a => 1)) == "{\"a\":1}"
    end

    @testset "exact numbers have one text form" begin
        @test exact_text(3) == "3"
        @test exact_text(-7 // 2) == "-7/2"
        @test exact_text(6 // 3) == "2"
        @test exact_text(big(10)^30 // 3) == "1000000000000000000000000000000/3"
        @test parse_exact("3/4") == 3 // 4
        @test parse_exact("6/8") == 3 // 4
        @test parse_exact("-12") == -12
        @test parse_exact("0.25") == 1 // 4
        @test parse_exact("-1.5e-3") == -3 // 2000
        @test parse_exact("2e3") == 2000
        @test parse_exact("123456789012345678901234567890") == big(123456789012345678901234567890)
        for bad in ("", "1/0", "1/-2", "+1", " 1", "1 ", "a", "1/2/3", "1.", ".5", "--1", "1e", "1e99999")
            @test_throws ArgumentError parse_exact(bad)
        end
        @test exact(0.5) == 1 // 2
        @test exact(0.1) != 1 // 10
        @test exact("7/3") == 7 // 3
        @test exact(3) isa Exact && exact(1 // 3) isa Exact
        @test_throws ArgumentError exact(NaN)
        rng = SplitMix(5)
        for _ in 1:500
            x = big(integer!(rng, -10^9:10^9)) // big(integer!(rng, 1:10^6))
            @test parse_exact(exact_text(x)) == x
        end
    end

    @testset "expressions are read with Lean's precedence" begin
        @test parse_expression("1 + 2 * x") == 1 + 2 * X
        @test parse_expression("(1 + 2) * x") == (Num(1) + 2) * X
        @test parse_expression("x - y - 1") == (X - Y) - 1
        @test parse_expression("x / y / 2") == (X / Y) / 2
        @test parse_expression("-x^2") == Neg(Pow(X, 2))
        @test parse_expression("-x * y") == Neg(X) * Y
        @test parse_expression("2 * -x") == 2 * Neg(X)
        @test parse_expression("-3") == Num(-3)
        @test parse_expression("--3") == Neg(Num(-3))
        @test parse_expression("-(3)") == Neg(Num(3))
        @test parse_expression("-0") == Neg(Num(0))
        @test parse_expression("-2^2") == Neg(Pow(Num(2), 2))
        @test parse_expression("x − 1") == X - 1
        @test parse_expression("0.25") == Num(1 // 4)
        @test parse_expression("1e2") == Num(100)
        @test parse_expression("sqrt(x + 1)") == Call(:sqrt, Node[X + 1])
        @test parse_expression("min(x, 2)") == Call(:min, Node[X, Num(2)])
        @test parse_expression("x₁ + y'") == Var("x₁") + Var("y'")
        @test parse_expression("α * β_2") == Var("α") * Var("β_2")
        for (bad, position) in (("", 1), ("x +", 4), ("2 x", 3), ("x ^ y", 5), ("x ^ 2 ^ 3", 7), ("foo(x)", 1),
                                ("sqrt(x, y)", 1), ("(x", 3), ("x)", 2), ("x # y", 3), ("x ^ -1", 5), ("min(x)", 1),
                                ("x <= 1", 3), ("1.5.2", 4))
            error = try
                parse_expression(bad)
            catch e
                e
            end
            @test error isa ParseError
            @test error.position == position
        end
        @test occursin("character 3", sprint(showerror, ParseError("m", 3)))
    end

    @testset "relations and conjunctions" begin
        @test parse_relation("2*x + 3*y <= 6") == Relation(:le, 2 * X + 3 * Y, Num(6))
        @test [parse_relation("x $(s) y").op for s in ("<=", "≤", "<", ">=", "≥", ">", "=", "==", "!=", "≠")] ==
              [:le, :le, :lt, :ge, :ge, :gt, :eq, :eq, :ne, :ne]
        @test parse_conjunction("0 <= x < 1") == [Relation(:le, Num(0), X), Relation(:lt, X, Num(1))]
        @test length(parse_conjunction("x > 0 ∧ y > 0 and x + y = 1 && x != y /\\ 0 <= y <= 1")) == 6
        @test_throws ParseError parse_relation("x + 1")
        @test_throws ParseError parse_relation("0 <= x <= 1")
        @test_throws ParseError parse_conjunction("x > 0 ∧")
        @test negate(parse_relation("x <= y")) == parse_relation("x > y")
        @test negate(negate(parse_relation("x = y"))) == parse_relation("x = y")
        @test variables(parse_conjunction("b > a and c + a = sqrt(b)")) == ["a", "b", "c"]
        @test variables(Num(1)) == String[]
    end

    @testset "printing" begin
        @test expression_text(parse_expression("(1 + 2) * x - -y")) == "(1 + 2) * x - -y"
        @test expression_text(parse_expression("x - (y - 1)")) == "x - (y - 1)"
        @test expression_text(parse_expression("x - y - 1")) == "x - y - 1"
        @test expression_text(parse_expression("-(x + 1)^2")) == "-(x + 1)^2"
        @test expression_text(parse_expression("(-x)^2")) == "(-x)^2"
        @test expression_text(Num(-3 // 4) * X) == "-3 / 4 * x"
        @test expression_text(X * Num(-3 // 4)) == "x * (-3 / 4)"
        @test expression_text(Pow(Num(1 // 2), 2)) == "(1 / 2)^2"
        @test expression_text(Neg(Num(-3))) == "-(-3)"
        @test expression_text(Pow(Num(-2), 3)) == "(-2)^3"
        @test expression_text(parse_expression("max(x, abs(y))")) == "max(x, abs(y))"
        @test relation_text(parse_relation("x*y >= 1/2")) == "x * y >= 1 / 2"
        @test relation_text(parse_relation("x*y >= 1/2"); style = :lean) == "x * y ≥ 1 / 2"
        @test relation_text(parse_relation("x != 0"); style = :lean) == "x ≠ 0"
        @test expression_text(parse_expression("sqrt(x + 1)^2 + abs(x - y) + min(x, y + 1)"); style = :lean) ==
              "(Real.sqrt (x + 1)) ^ 2 + |x - y| + (min x (y + 1))"
        @test sprint(show, X + 1) == "x + 1"
        @test sprint(show, parse_relation("x<1")) == "x < 1"
        @test_throws ArgumentError expression_text(X; style = :latex)
    end

    @testset "printing and reading agree on random expressions" begin
        rng = SplitMix(2026)
        env = Dict("x" => 3 // 7, "y" => -5 // 2)
        for _ in 1:500
            node = random_node(rng, 4)
            reread = parse_expression(expression_text(node))
            @test evaluate(reread, env) == evaluate(node, env)
            @test parse_expression(expression_text(reread)) == reread
            @test expression_text(reread) == expression_text(node)
        end
    end

    @testset "evaluation" begin
        env = Dict("x" => 3, "y" => 1 // 2)
        @test evaluate(parse_expression("x^2 + y / 4 - 1"), env) == 9 + 1 // 8 - 1
        @test evaluate(parse_expression("x^2 + y / 4 - 1"), env) isa Exact
        @test evaluate(parse_expression("abs(y - x) + min(x, y) * max(x, y)"), env) == 5 // 2 + 3 // 2
        @test evaluate(parse_expression("x^0"), env) == 1
        @test evaluate(parse_expression("sqrt(x + 1) + exp(0) + log(1) + sin(0) + cos(0)"), env, Float64) == 4.0
        @test evaluate(parse_expression("0.1 + 0.2"), env) == 3 // 10
        @test evaluate(parse_expression("0.1 + 0.2"), env, Float64) == 0.1 + 0.2
        @test evaluate(parse_expression("x / 2"), Object("x" => 1)) == 1 // 2
        @test_throws DivideError evaluate(parse_expression("1 / (x - 3)"), env)
        @test_throws UnboundVariable evaluate(parse_expression("z"), env)
        @test_throws InexactFunction evaluate(parse_expression("sqrt(x)"), env)
        @test_throws DomainError evaluate(parse_expression("sqrt(0 - x)"), env, Float64)
        @test_throws DomainError evaluate(parse_expression("log(0)"), env, Float64)
        @test occursin("\"z\"", sprint(showerror, UnboundVariable("z")))
        @test occursin("sqrt", sprint(showerror, InexactFunction(:sqrt)))
        @test holds(parse_relation("x > y"), env)
        @test !holds(parse_relation("x = y"), env)
        @test holds(parse_relation("x * y <= 1.5"), env, Float64)
        @test all(holds(r, env) != holds(negate(r), env) for r in parse_conjunction("x < y and x >= 3 and y != 1/2"))
        @test substitute(parse_expression("x + y"), Dict("x" => parse_expression("y * 2"))) == Y * 2 + Y
        @test substitute(parse_relation("x < 1"), Dict("x" => 5)) == Relation(:lt, Num(5), Num(1))
    end

    @testset "documents name their schema" begin
        doc = document("maithic.oracle.certificate/1", "kind" => "discrete.bezout", "n" => 1)
        @test keys(doc) == ["schema", "kind", "n"]
        @test schema_of(doc) == "maithic.oracle.certificate/1"
        @test schema_of(Any[]) === nothing
        @test expect_schema(doc, "maithic.oracle.certificate/1") === doc
        @test_throws ArgumentError expect_schema(doc, "maithic.oracle.goal/1")
        @test_throws ArgumentError expect_schema(Object(), "maithic.oracle.goal/1")
        record = engine_record("MaithicOracle", v"0.1.0", "pivot" => "bland")
        @test write_json(record) == "{\"name\":\"MaithicOracle\",\"version\":\"0.1.0\",\"parameters\":{\"pivot\":\"bland\"}}"
    end

    @testset "the random stream is fixed by its definition" begin
        rng = SplitMix(0)
        @test [next_u64!(rng) for _ in 1:3] == [0xe220a8397b1dcdaf, 0x6e789e6aa1b965f4, 0x06c45d188009454f]
        @test [next_u64!(SplitMix(-1)) for _ in 1:2] == [next_u64!(SplitMix(typemax(UInt64))) for _ in 1:2]
        a, b = SplitMix(42), SplitMix(42)
        @test [uniform!(a) for _ in 1:20] == [uniform!(b) for _ in 1:20]
        rng = SplitMix(7)
        draws = [uniform!(rng) for _ in 1:2000]
        @test all(0 .<= draws .< 1) && 0.45 < sum(draws) / length(draws) < 0.55
        @test all(2 <= uniform!(rng, 2, 5) < 5 for _ in 1:200)
        counts = zeros(Int, 6)
        for _ in 1:6000
            counts[integer!(rng, 1:6)] += 1
        end
        @test all(850 .< counts .< 1150)
        @test Set(integer!(rng, -2:2) for _ in 1:200) == Set(-2:2)
        @test integer!(rng, 5:5) == 5
        @test_throws ArgumentError integer!(rng, 1:0)
        items = collect(1:20)
        MaithicCore.shuffle!(rng, items)
        @test sort(items) == collect(1:20) && items != collect(1:20)
    end
end
