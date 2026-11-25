# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Facts of modular arithmetic, each with the number that makes it checkable.

"""
    InverseCertificate(a, modulus, inverse)

`a * inverse` leaves remainder 1 on division by `modulus`.
"""
struct InverseCertificate <: Certificate
    a::BigInt
    modulus::BigInt
    inverse::BigInt
end

kind(::InverseCertificate) = "discrete.inverse"
claim(c::InverseCertificate) = "$(c.a) * $(c.inverse) ≡ 1 [ZMOD $(c.modulus)]"
certificate_data(c::InverseCertificate) =
    Object("a" => string(c.a), "modulus" => string(c.modulus), "inverse" => string(c.inverse))
check(c::InverseCertificate) = c.modulus >= 2 && 0 <= c.inverse < c.modulus && mod(c.a * c.inverse, c.modulus) == 1
lean_statement(c::InverseCertificate) =
    "example : ($(lean_integer(c.a)) * $(c.inverse) : Int) % $(c.modulus) = 1 := by decide\n"

"The inverse of `a` modulo `modulus` with its certificate, or `nothing` when `a` is not invertible."
function modular_inverse(a::Integer, modulus::Integer)
    modulus >= 2 || throw(DomainError(modulus, "a modulus is at least 2"))
    g, x, _ = gcdx(BigInt(a), BigInt(modulus))
    g == 1 ? InverseCertificate(BigInt(a), BigInt(modulus), mod(x, BigInt(modulus))) : nothing
end

"""
    OrderCertificate(a, modulus, order, factors, proof)

The multiplicative order of `a` modulo `modulus` is `order`: `a^order = 1`,
and `a^(order/q) != 1` for every prime `q` dividing `order`. `factors` is the
prime factorization of `order` and `proof` is a Pratt proof for its primes,
so that no prime divisor can have been left out.
"""
struct OrderCertificate <: Certificate
    a::BigInt
    modulus::BigInt
    order::BigInt
    factors::Vector{Tuple{BigInt,Int}}
    proof::Vector{PrattEntry}
end

kind(::OrderCertificate) = "discrete.order"
claim(c::OrderCertificate) = "orderOf ($(c.a) : ZMod $(c.modulus)) = $(c.order)"
certificate_data(c::OrderCertificate) =
    Object("a" => string(c.a), "modulus" => string(c.modulus), "order" => string(c.order),
           "factors" => factors_data(c.factors), "proof" => proof_data(c.proof))

function check(c::OrderCertificate)
    known = certified_primes(c.proof)
    known !== nothing && c.modulus >= 2 && c.order >= 1 && allunique(first.(c.factors)) &&
        all(e >= 1 && q in known for (q, e) in c.factors) && factors_product(c.factors) == c.order &&
        powermod(c.a, c.order, c.modulus) == 1 &&
        all(powermod(c.a, c.order ÷ q, c.modulus) != 1 for (q, _) in c.factors)
end

"""
    multiplicative_order(a, modulus; budget = 4_000_000)

The multiplicative order of `a` modulo `modulus` with its certificate, or
`nothing` when `a` is not a unit. The order is found by factoring the modulus,
computing the size of the unit group and stripping every prime that can be
stripped.
"""
function multiplicative_order(a::Integer, modulus::Integer; budget::Int = 4_000_000)
    a, modulus = BigInt(a), BigInt(modulus)
    modulus >= 2 || throw(DomainError(modulus, "a modulus is at least 2"))
    gcd(a, modulus) == 1 || return nothing
    order = prod((p^(e - 1) * (p - 1) for (p, e) in prime_factors(modulus; budget)); init = big(1))
    for (q, _) in prime_factors(order; budget)
        while order % q == 0 && powermod(a, order ÷ q, modulus) == 1
            order ÷= q
        end
    end
    factors = prime_factors(order; budget)
    proof = pratt_proof(first.(factors); budget)
    proof === nothing && throw(FactoringBudget(order))
    OrderCertificate(a, modulus, order, factors, proof)
end

"""
    LogarithmCertificate(base, target, modulus, exponent)

`base^exponent` and `target` leave the same remainder on division by `modulus`.
"""
struct LogarithmCertificate <: Certificate
    base::BigInt
    target::BigInt
    modulus::BigInt
    exponent::BigInt
end

kind(::LogarithmCertificate) = "discrete.logarithm"
claim(c::LogarithmCertificate) = "$(c.base) ^ $(c.exponent) ≡ $(c.target) [ZMOD $(c.modulus)]"
certificate_data(c::LogarithmCertificate) =
    Object("base" => string(c.base), "target" => string(c.target), "modulus" => string(c.modulus),
           "exponent" => string(c.exponent))
check(c::LogarithmCertificate) =
    c.modulus >= 2 && c.exponent >= 0 && powermod(c.base, c.exponent, c.modulus) == mod(c.target, c.modulus)

"""
    discrete_logarithm(base, target, modulus; limit = 2^40)

The least exponent `x >= 0` below `limit` with `base^x = target` modulo
`modulus`, with its certificate; or `nothing` when there is none below the
limit.

The modulus is split into the part built from the primes of the base and the
part coprime to it. Modulo the first part the powers of the base reach zero
after a few steps and stay there, so those few exponents are tried one by
one; modulo the second part the base is a unit and the baby-step giant-step
method applies.
"""
function discrete_logarithm(base::Integer, target::Integer, modulus::Integer; limit::Integer = big(2)^40)
    base, target, modulus, limit = BigInt(base), BigInt(target), BigInt(modulus), BigInt(limit)
    modulus >= 2 || throw(DomainError(modulus, "a modulus is at least 2"))
    target = mod(target, modulus)
    found(x) = LogarithmCertificate(base, target, modulus, BigInt(x))
    unit_part = modulus
    d = gcd(base, unit_part)
    while d > 1
        unit_part ÷= d
        d = gcd(base, unit_part)
    end
    smooth_part = modulus ÷ unit_part
    tail, power = big(0), mod(big(1), modulus)
    while mod(power, smooth_part) != 0
        tail < limit || return nothing
        power == target && return found(tail)
        power = mod(power * base, modulus)
        tail += 1
    end
    (tail < limit && mod(target, smooth_part) == 0) || return nothing
    unit_part == 1 && return found(tail)
    # From here on every power is zero modulo the smooth part, and so is the target.
    shifted = mod(target * invmod(powermod(base, tail, unit_part), unit_part), unit_part)
    span = min(limit - tail, unit_part)
    step = isqrt(span) + 1
    step <= 4_000_000 || throw(ArgumentError("the search space is too large for baby-step giant-step"))
    table = Dict{BigInt,BigInt}()
    power = mod(big(1), unit_part)
    for j in 0:(step - 1)
        get!(table, power, big(j))
        power = mod(power * base, unit_part)
    end
    giant = invmod(powermod(base, step, unit_part), unit_part)
    current = shifted
    for i in 0:step
        if haskey(table, current)
            x = i * step + table[current]
            return x < span ? found(tail + x) : nothing
        end
        current = mod(current * giant, unit_part)
    end
    nothing
end

"""
    SquareRootCertificate(a, modulus, root)

`root^2 - a` is divisible by `modulus`.
"""
struct SquareRootCertificate <: Certificate
    a::BigInt
    modulus::BigInt
    root::BigInt
end

kind(::SquareRootCertificate) = "discrete.square_root"
claim(c::SquareRootCertificate) = "$(c.root) ^ 2 ≡ $(c.a) [ZMOD $(c.modulus)]"
certificate_data(c::SquareRootCertificate) =
    Object("a" => string(c.a), "modulus" => string(c.modulus), "root" => string(c.root))
check(c::SquareRootCertificate) = c.modulus >= 2 && 0 <= c.root < c.modulus && mod(c.root^2 - c.a, c.modulus) == 0
lean_statement(c::SquareRootCertificate) =
    "example : (($(c.root) : Int) ^ 2 - $(lean_integer(c.a))) % $(c.modulus) = 0 := by decide\n"

"A square root of `a` modulo the odd prime `p`, by the Tonelli-Shanks method; `nothing` for a non-residue."
function tonelli_shanks(a::BigInt, p::BigInt)
    a = mod(a, p)
    a == 0 && return big(0)
    powermod(a, (p - 1) ÷ 2, p) == 1 || return nothing
    q = p - 1
    s = trailing_zeros(q)
    q >>= s
    z = big(2)
    while powermod(z, (p - 1) ÷ 2, p) != p - 1
        z += 1
    end
    m, c, t, r = s, powermod(z, q, p), powermod(a, q, p), powermod(a, (q + 1) ÷ 2, p)
    while t != 1
        i, probe = 0, t
        while probe != 1
            probe = probe * probe % p
            i += 1
        end
        b = powermod(c, big(2)^(m - i - 1), p)
        m, c = i, b * b % p
        t, r = t * c % p, r * b % p
    end
    r
end

"""
    modular_square_root(a, modulus; exhaustive_below = 2_000_000)

A square root of `a` modulo `modulus` with its certificate, or `nothing` when
none is found. For an odd prime modulus the Tonelli-Shanks method decides the
question; for another modulus below `exhaustive_below` every residue is
tried, so `nothing` again means there is no root; above that the answer
`nothing` only means that this function does not know.
"""
function modular_square_root(a::Integer, modulus::Integer; exhaustive_below::Integer = 2_000_000)
    a, modulus = BigInt(a), BigInt(modulus)
    modulus >= 2 || throw(DomainError(modulus, "a modulus is at least 2"))
    root = if modulus > 2 && is_probable_prime(modulus)
        tonelli_shanks(a, modulus)
    elseif modulus < min(exhaustive_below, 2^31)
        m, target = Int(modulus), Int(mod(a, modulus))
        found = findfirst(r -> r * r % m == target, 0:(m - 1))
        found === nothing ? nothing : big(found - 1)
    else
        nothing
    end
    root === nothing ? nothing : SquareRootCertificate(a, modulus, min(root, mod(-root, modulus)))
end

READERS["discrete.inverse"] = d -> InverseCertificate(big_of(d["a"]), big_of(d["modulus"]), big_of(d["inverse"]))
READERS["discrete.order"] = d -> OrderCertificate(big_of(d["a"]), big_of(d["modulus"]), big_of(d["order"]),
                                                  factors_of(d["factors"]), proof_of(d["proof"]))
READERS["discrete.logarithm"] = d -> LogarithmCertificate(big_of(d["base"]), big_of(d["target"]), big_of(d["modulus"]),
                                                          big_of(d["exponent"]))
READERS["discrete.square_root"] = d -> SquareRootCertificate(big_of(d["a"]), big_of(d["modulus"]), big_of(d["root"]))
