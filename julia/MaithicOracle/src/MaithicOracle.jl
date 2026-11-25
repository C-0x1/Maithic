# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicOracle

Computation that settles goals, and the certificates that let it be believed.

A solver can decide in a moment what a proof search would take minutes to
find, but a solver is a large program and its answer is only a claim. Every
function of this package that proves something returns a *certificate*: a
small piece of data from which the fact can be checked by arithmetic alone.
The certificate is checked here before it is written ([`check`](@ref)), and
it is checked again by Lean before anything is called verified. Julia
computes; the kernel decides.

Four families of goals are covered:

- `discrete`: primality (Pratt certificates), factorizations, modular
  inverses, orders, logarithms and square roots, Bézout coefficients, and
  witnesses for existential goals over the integers;
- `linear`: linear arithmetic over the rationals and the integers, by an
  exact simplex method that returns Farkas multipliers;
- `poly`: polynomial identities and ideal membership, by Buchberger's
  algorithm with cofactors;
- `interval`: inequalities between real functions on a box, by interval
  arithmetic with outward rounding and subdivision.

Goals arrive in the oracle exchange format (`maithic.oracle.goal/1`) and
certificates leave as `maithic.oracle.certificate/1`; both are described in
`docs/reference/oracle-format.md`.
"""
module MaithicOracle

using MaithicCore

export Certificate, Outcome, Proved, Refuted, Unknown, RejectedCertificate
export check, kind, claim, certificate_document, certificate_json, read_certificate, lean_check,
       lean_replay, LEAN_REPLAY_IMPORT, lean_scripts, answer
export Hypothesis, Goal, read_goal, goal_document, solve, outcome_document
export is_probable_prime, prime_factors, FactoringBudget
export PrattEntry, PrimeCertificate, prove_prime, FactorizationCertificate, factorization
export InverseCertificate, modular_inverse, OrderCertificate, multiplicative_order,
       LogarithmCertificate, discrete_logarithm, SquareRootCertificate, modular_square_root
export BezoutCertificate, bezout, WitnessCertificate, find_witness
export NotLinear, LinearForm, linear_form, Row, linear_row, tighten, refutes, SimplexResult, simplex, feasibility
export Refutation, LinearCertificate, prove_linear, negation_branches
export NotPolynomial, Monomial, Polynomial, polynomial, polynomial_text, polynomial_node, monomial_less, degree,
       evaluate_polynomial, divide, groebner, membership, Budget, Tracked, nonroot
export PolynomialCertificate, prove_polynomial
export RealInterval, Undefined, enclose, enclosure, width, exact_bounds
export BoxClaim, IntervalCertificate, variable_domain, tiles, simple_enclosure, prove_real

const VERSION = v"0.1.0"

include("certificate/base.jl")
include("certificate/registry.jl")
include("certificate/goal.jl")
include("discrete/numbers.jl")
include("discrete/primes.jl")
include("discrete/modular.jl")
include("discrete/bezout.jl")
include("discrete/witness.jl")
include("linear/form.jl")
include("linear/simplex.jl")
include("linear/certificate.jl")
include("linear/prove.jl")
include("poly/polynomial.jl")
include("poly/groebner.jl")
include("poly/certificate.jl")
include("interval/arithmetic.jl")
include("interval/prove.jl")

end # module
