# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Exact numbers and their text form.

"The number type of every certificate: a ratio of integers of any size."
const Exact = Rational{BigInt}

"""
    exact(x)

`x` as an [`Exact`](@ref) number, with no rounding. A floating-point number
is converted to the ratio it stands for exactly, so `exact(0.1)` is not
`1//10`; text is read with [`parse_exact`](@ref).
"""
exact(x::Integer) = Exact(BigInt(x))
exact(x::Rational) = Exact(x)
exact(x::AbstractString) = parse_exact(x)

function exact(x::AbstractFloat)
    isfinite(x) || throw(ArgumentError("$(x) is not a number that can be made exact"))
    Exact(x)
end

"""
    exact_text(x)

The text form of an integer or a rational: `"3"`, `"-7/2"`. The fraction is in
lowest terms, the denominator is positive and is omitted when it is one, so
each number has one text form. This is the encoding of numbers in every
certificate schema.
"""
exact_text(x::Integer) = string(x)
exact_text(x::Rational) =
    denominator(x) == 1 ? string(numerator(x)) : string(numerator(x), "/", denominator(x))

"""
    parse_exact(text)

Reads the text form of an exact number: an integer (`"-12"`), a fraction
(`"3/4"`, not necessarily in lowest terms) or a decimal (`"0.25"`, `"1.5e-3"`),
each read exactly. Anything else, including a zero denominator, is an
`ArgumentError`.
"""
function parse_exact(text::AbstractString)::Exact
    m = match(r"^(-?[0-9]+)(?:/([0-9]+))?$", text)
    if m !== nothing
        n = parse(BigInt, m.captures[1])
        d = m.captures[2] === nothing ? big(1) : parse(BigInt, m.captures[2])
        d == 0 && throw(ArgumentError("zero denominator in \"$(text)\""))
        return n // d
    end
    m = match(r"^(-?)([0-9]+)(?:\.([0-9]+))?(?:[eE]([+-]?[0-9]+))?$", text)
    m === nothing && throw(ArgumentError("\"$(text)\" is not an exact number"))
    fraction = m.captures[3] === nothing ? "" : m.captures[3]
    exponent = m.captures[4] === nothing ? 0 : parse(Int, m.captures[4])
    abs(exponent) <= 10_000 || throw(ArgumentError("the exponent of \"$(text)\" is out of range"))
    digits = parse(BigInt, string(m.captures[2], fraction))
    scale = exponent - length(fraction)
    value = scale >= 0 ? Exact(digits * big(10)^scale) : digits // big(10)^(-scale)
    m.captures[1] == "-" ? -value : value
end
