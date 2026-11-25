# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Greatest common divisors with the coefficients that prove them.

"""
    BezoutCertificate(a, b, g, x, y, a_cofactor, b_cofactor)

`g` is the greatest common divisor of `a` and `b`. The cofactors show that
`g` divides both (`g * a_cofactor = a`, `g * b_cofactor = b`), and the
identity `a * x + b * y = g` shows that every common divisor divides `g`.
"""
struct BezoutCertificate <: Certificate
    a::BigInt
    b::BigInt
    g::BigInt
    x::BigInt
    y::BigInt
    a_cofactor::BigInt
    b_cofactor::BigInt
end

kind(::BezoutCertificate) = "discrete.bezout"
claim(c::BezoutCertificate) = "Int.gcd $(lean_integer(c.a)) $(lean_integer(c.b)) = $(c.g)"
certificate_data(c::BezoutCertificate) =
    Object("a" => string(c.a), "b" => string(c.b), "gcd" => string(c.g), "x" => string(c.x), "y" => string(c.y),
           "a_cofactor" => string(c.a_cofactor), "b_cofactor" => string(c.b_cofactor))
check(c::BezoutCertificate) =
    c.g >= 0 && c.a * c.x + c.b * c.y == c.g && c.g * c.a_cofactor == c.a && c.g * c.b_cofactor == c.b

lean_statement(c::BezoutCertificate) =
    string("example : ($(lean_integer(c.a)) : Int) * $(lean_integer(c.x)) + $(lean_integer(c.b)) * ",
           "$(lean_integer(c.y)) = $(c.g) ∧ ($(c.g) : Int) * $(lean_integer(c.a_cofactor)) = $(lean_integer(c.a)) ∧ ",
           "($(c.g) : Int) * $(lean_integer(c.b_cofactor)) = $(lean_integer(c.b)) := by decide\n")

"""
    bezout(a, b)

The greatest common divisor of two integers with Bézout coefficients and
cofactors, as a [`BezoutCertificate`](@ref). The coefficients come from the
extended Euclidean algorithm, written out here so that every step is an
exact integer step.
"""
function bezout(a::Integer, b::Integer)
    a, b = BigInt(a), BigInt(b)
    r0, r1 = a, b
    x0, x1, y0, y1 = big(1), big(0), big(0), big(1)
    while r1 != 0
        quotient = fld(r0, r1)
        r0, r1 = r1, r0 - quotient * r1
        x0, x1 = x1, x0 - quotient * x1
        y0, y1 = y1, y0 - quotient * y1
    end
    r0 < 0 && ((r0, x0, y0) = (-r0, -x0, -y0))
    BezoutCertificate(a, b, r0, x0, y0, r0 == 0 ? big(0) : a ÷ r0, r0 == 0 ? big(0) : b ÷ r0)
end

READERS["discrete.bezout"] = d -> BezoutCertificate(big_of(d["a"]), big_of(d["b"]), big_of(d["gcd"]), big_of(d["x"]),
                                                    big_of(d["y"]), big_of(d["a_cofactor"]), big_of(d["b_cofactor"]))
