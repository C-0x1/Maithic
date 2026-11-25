# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Number theory that finds things. Nothing here is trusted: every result is
# turned into a certificate and the certificate is checked.

"The primes below 1000, for trial division."
const SMALL_PRIMES = let sieve = trues(999)
    sieve[1] = false
    for i in 2:31
        sieve[i] && (sieve[(i * i):i:999] .= false)
    end
    BigInt[i for i in 1:999 if sieve[i]]
end

"Bases that make the Miller-Rabin test exact for every number below 3.3 * 10^24."
const WITNESS_BASES = SMALL_PRIMES[1:12]

"""
    is_probable_prime(n)

A fast primality test: trial division by the primes below 1000, then
Miller-Rabin to twelve bases. It never calls a prime composite, and it is
exact below 3.3 * 10^24. It is used to steer factoring; a proof of primality
is a [`PrimeCertificate`](@ref).
"""
function is_probable_prime(n::Integer)
    n = BigInt(n)
    n < 2 && return false
    for p in SMALL_PRIMES
        n == p && return true
        n % p == 0 && return false
    end
    d = n - 1
    s = trailing_zeros(d)
    d >>= s
    for a in WITNESS_BASES
        x = powermod(a, d, n)
        (x == 1 || x == n - 1) && continue
        composite = true
        for _ in 1:(s - 1)
            x = powermod(x, 2, n)
            if x == n - 1
                composite = false
                break
            end
        end
        composite && return false
    end
    true
end

"""
    FactoringBudget(n)

The factoring routine gave up on the composite number `n` within its budget
of steps.
"""
struct FactoringBudget <: Exception
    n::BigInt
end

Base.showerror(io::IO, e::FactoringBudget) = print(io, "no factor of ", e.n, " was found within the step budget")

"A proper factor of the odd composite `n`, by Brent's variant of Pollard's rho method."
function proper_factor(n::BigInt, rng::SplitMix, budget::Int)
    steps = 0
    while steps < budget
        y = BigInt(integer!(rng, 0:(10^9))) % n
        c = BigInt(integer!(rng, 1:(10^9))) % n
        c == 0 && (c = big(1))
        g, r, q = big(1), 1, big(1)
        x = ys = y
        while g == 1 && steps < budget
            x = y
            for _ in 1:r
                y = (y * y + c) % n
            end
            k = 0
            while k < r && g == 1
                ys = y
                for _ in 1:min(64, r - k)
                    y = (y * y + c) % n
                    q = q * abs(x - y) % n
                end
                g = gcd(q, n)
                k += 64
                steps += 64
            end
            r *= 2
        end
        if g == n
            g = big(1)
            while g == 1
                ys = (ys * ys + c) % n
                g = gcd(abs(x - ys), n)
            end
        end
        1 < g < n && return g
    end
    throw(FactoringBudget(n))
end

function collect_factors!(found::Dict{BigInt,Int}, n::BigInt, rng::SplitMix, budget::Int)
    n == 1 && return found
    if is_probable_prime(n)
        found[n] = get(found, n, 0) + 1
        return found
    end
    root = isqrt(n)
    if root * root == n
        collect_factors!(found, root, rng, budget)
        return collect_factors!(found, root, rng, budget)
    end
    g = proper_factor(n, rng, budget)
    collect_factors!(found, g, rng, budget)
    collect_factors!(found, n ÷ g, rng, budget)
end

"""
    prime_factors(n; budget = 4_000_000, seed = 1)

The prime factorization of a positive integer as a sorted list of
`(prime, exponent)`: trial division by the primes below 1000, then Pollard's
rho method for what remains. The primes are probable primes; a
[`FactorizationCertificate`](@ref) proves them. A number whose factors resist
`budget` steps is a [`FactoringBudget`](@ref) error.
"""
function prime_factors(n::Integer; budget::Int = 4_000_000, seed::Integer = 1)
    n = BigInt(n)
    n >= 1 || throw(DomainError(n, "only positive integers have a factorization"))
    found = Dict{BigInt,Int}()
    for p in SMALL_PRIMES
        p * p > n && break
        while n % p == 0
            found[p] = get(found, p, 0) + 1
            n ÷= p
        end
    end
    collect_factors!(found, n, SplitMix(seed), budget)
    sort!([(p, e) for (p, e) in found]; by = first)
end

factors_product(factors) = prod((p^e for (p, e) in factors); init = big(1))
factors_data(factors) = Any[Any[string(p), e] for (p, e) in factors]

function factors_of(value)
    factors = Tuple{BigInt,Int}[]
    for entry in list_of(value)
        (entry isa AbstractVector && length(entry) == 2) || throw(ArgumentError("a factor is a pair of prime and exponent"))
        push!(factors, (big_of(entry[1]), count_of(entry[2])))
    end
    factors
end

"`n` in Lean's notation: negative numbers in parentheses."
lean_integer(n::Integer) = n < 0 ? string("(", n, ")") : string(n)

"A product of prime powers in Lean's notation, `2 ^ 3 * 5 ^ 1`."
lean_product(factors) = isempty(factors) ? "1" : join((string(p, " ^ ", e) for (p, e) in factors), " * ")

"A product of prime powers as mathematics is usually written, `2^3 * 5`."
plain_product(factors) = isempty(factors) ? "1" : join((e == 1 ? string(p) : string(p, "^", e) for (p, e) in factors), " * ")
