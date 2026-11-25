# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Primality and factorization certificates.

"""
    PrattEntry(n, witness, factors)

One line of a Pratt proof: `n` is prime because `witness` has order `n - 1`
modulo `n`. `factors` is the prime factorization of `n - 1`; the line is
valid once every prime in it is 2 or has a line of its own earlier in the
proof.
"""
struct PrattEntry
    n::BigInt
    witness::BigInt
    factors::Vector{Tuple{BigInt,Int}}
end

Base.:(==)(a::PrattEntry, b::PrattEntry) = a.n == b.n && a.witness == b.witness && a.factors == b.factors

"""
    certified_primes(proof)

Checks the lines of a Pratt proof in order and returns the set of numbers it
proves prime (2 is always among them), or `nothing` as soon as a line fails.

A line `(n, a, factors)` is accepted when the factors multiply to `n - 1`,
each is already proved prime, `a^(n-1) = 1` and `a^((n-1)/q) != 1` modulo `n`
for every prime factor `q`. Then the order of `a` is exactly `n - 1`, so
there are `n - 1` units modulo `n`, which happens only for a prime (Lucas).
"""
function certified_primes(proof::AbstractVector{PrattEntry})
    known = Set{BigInt}([big(2)])
    for entry in proof
        entry.n > 2 || return nothing
        all(e >= 1 && q in known for (q, e) in entry.factors) || return nothing
        allunique(first.(entry.factors)) || return nothing
        factors_product(entry.factors) == entry.n - 1 || return nothing
        1 < entry.witness < entry.n || return nothing
        powermod(entry.witness, entry.n - 1, entry.n) == 1 || return nothing
        all(powermod(entry.witness, (entry.n - 1) ÷ q, entry.n) != 1 for (q, _) in entry.factors) || return nothing
        push!(known, entry.n)
    end
    known
end

"Appends to `proof` the lines that prove `n` prime; `false` if `n` turns out to be composite."
function pratt!(proof::Vector{PrattEntry}, done::Set{BigInt}, n::BigInt, budget::Int)
    (n == 2 || n in done) && return true
    (n < 2 || !is_probable_prime(n)) && return false
    factors = prime_factors(n - 1; budget)
    for (q, _) in factors
        pratt!(proof, done, q, budget) || return false
    end
    for a in Iterators.flatten((SMALL_PRIMES, big(1000):min(n - 1, big(100_000))))
        a >= n && break
        if powermod(a, n - 1, n) == 1 && all(powermod(a, (n - 1) ÷ q, n) != 1 for (q, _) in factors)
            push!(proof, PrattEntry(n, a, factors))
            push!(done, n)
            return true
        end
    end
    false
end

"The Pratt lines that prove every number in `primes` prime, or `nothing` if one of them is not."
function pratt_proof(primes; budget::Int = 4_000_000)
    proof = PrattEntry[]
    done = Set{BigInt}()
    for p in primes
        pratt!(proof, done, BigInt(p), budget) || return nothing
    end
    proof
end

entry_data(e::PrattEntry) = Object("n" => string(e.n), "witness" => string(e.witness), "factors" => factors_data(e.factors))
proof_data(proof) = Any[entry_data(e) for e in proof]
proof_of(value) = PrattEntry[PrattEntry(big_of(e["n"]), big_of(e["witness"]), factors_of(e["factors"])) for e in list_of(value)]

"""
    PrimeCertificate(n, proof)

`n` is prime, by a Pratt proof whose lines are listed with the smaller primes
first.
"""
struct PrimeCertificate <: Certificate
    n::BigInt
    proof::Vector{PrattEntry}
end

kind(::PrimeCertificate) = "discrete.prime"
claim(c::PrimeCertificate) = "Nat.Prime $(c.n)"
certificate_data(c::PrimeCertificate) = Object("n" => string(c.n), "proof" => proof_data(c.proof))

function check(c::PrimeCertificate)
    known = certified_primes(c.proof)
    known !== nothing && c.n in known
end

"""
    prove_prime(n; budget = 4_000_000)

A [`PrimeCertificate`](@ref) for `n`, or `nothing` when `n` is not prime. The
proof needs the factorization of `n - 1` and, recursively, of each prime it
meets; a number for which that is out of reach is a
[`FactoringBudget`](@ref) error.
"""
function prove_prime(n::Integer; budget::Int = 4_000_000)
    proof = pratt_proof((n,); budget)
    proof === nothing ? nothing : PrimeCertificate(BigInt(n), proof)
end

"""
    FactorizationCertificate(n, factors, proof)

`n` is the product of the prime powers in `factors`, and `proof` is a Pratt
proof that every one of those primes is prime. Listing the primes in
increasing order makes the factorization unique, so the certificate also
settles what the prime factors of `n` are.
"""
struct FactorizationCertificate <: Certificate
    n::BigInt
    factors::Vector{Tuple{BigInt,Int}}
    proof::Vector{PrattEntry}
end

kind(::FactorizationCertificate) = "discrete.factorization"
claim(c::FactorizationCertificate) = "$(c.n) = $(plain_product(c.factors))"
certificate_data(c::FactorizationCertificate) =
    Object("n" => string(c.n), "factors" => factors_data(c.factors), "proof" => proof_data(c.proof))

function check(c::FactorizationCertificate)
    known = certified_primes(c.proof)
    known !== nothing && c.n >= 1 && issorted(first.(c.factors); lt = <=) &&
        all(e >= 1 && p in known for (p, e) in c.factors) && factors_product(c.factors) == c.n
end

lean_statement(c::FactorizationCertificate) =
    "example : ($(c.n) : Nat) = $(lean_product(c.factors)) := by decide\n"

"""
    factorization(n; budget = 4_000_000)

The prime factorization of a positive integer with a proof that each factor
is prime, as a [`FactorizationCertificate`](@ref).
"""
function factorization(n::Integer; budget::Int = 4_000_000)
    factors = prime_factors(n; budget)
    proof = pratt_proof(first.(factors); budget)
    proof === nothing && throw(FactoringBudget(BigInt(n)))
    FactorizationCertificate(BigInt(n), factors, proof)
end

READERS["discrete.prime"] = d -> PrimeCertificate(big_of(d["n"]), proof_of(d["proof"]))
READERS["discrete.factorization"] =
    d -> FactorizationCertificate(big_of(d["n"]), factors_of(d["factors"]), proof_of(d["proof"]))
