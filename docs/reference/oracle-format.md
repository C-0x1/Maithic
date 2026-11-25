# The oracle exchange format

Goals go to the oracles, and certificates and outcomes come back, as JSON documents. Each names its schema; the
schemas are in [`schemas/oracle/`](../../schemas/oracle), and [`fixtures/oracle/`](../../fixtures/oracle) holds
goals with the outcomes and certificates computed for them, which `make hygiene` validates.

## Goals: `maithic.oracle.goal/1`

```json
{
  "schema": "maithic.oracle.goal/1",
  "kind": "linear",
  "domain": "rat",
  "variables": ["x", "y"],
  "hypotheses": [{"name": "h1", "statement": "2 * x + 3 * y <= 6"}, "x >= 0", "y >= 0"],
  "goal": "x + y <= 3"
}
```

| Member | Meaning |
|---|---|
| `kind` | `linear`, `polynomial` or `real`: which oracle is asked. |
| `domain` | `int`, `rat` or `real`. Optional; `rat`, or `real` for the kind `real`. |
| `variables` | The variables, in the order certificates refer to them. Optional; the sorted names that occur. |
| `hypotheses` | Each an object with `name` and `statement`, or just the statement, then named `h1`, `h2`, … by position. |
| `goal` | A comparison, or `"false"` to ask for a refutation of the hypotheses. |

### Statements

A statement is a comparison of two expressions.

```text
comparison   <=  <  >=  >  =  !=        also written  ≤  ≥  ≠  ==
expression   numbers       3   0.25   1e-3      read exactly, never rounded
             variables     x   x1   x'   α
             operators     +  -  *  /  ^ with a natural-number exponent
             functions     sqrt exp log sin cos abs min max
             parentheses
```

Precedence is Lean's: `-x^2` is `-(x^2)` and `-x * y` is `(-x) * y`. A chain `0 <= x < 1` stands for its two
links, and comparisons may be joined with `∧`, `/\`, `&&` or `and` where a conjunction is expected. There is no
implicit multiplication: `2x` is an error, with the position of the error.

The linear oracle accepts what is linear: products with a constant side, quotients by a nonzero constant. The
polynomial oracle accepts what is polynomial. The real oracle accepts everything and needs every variable
bounded on both sides by hypotheses such as `0 <= x` and `x <= 1`.

## Outcomes: `maithic.oracle.outcome/1`

```json
{"schema": "maithic.oracle.outcome/1", "outcome": "proved", "certificate": { ... }}
{"schema": "maithic.oracle.outcome/1", "outcome": "refuted", "model": {"x": "3", "y": "0"}}
{"schema": "maithic.oracle.outcome/1", "outcome": "unknown", "reason": "the goal is not a polynomial combination of the hypotheses"}
```

A model assigns an exact value to every variable. It satisfies every hypothesis and violates the goal, and the
oracle has verified that before reporting it.

## Certificates: `maithic.oracle.certificate/1`

```json
{
  "schema": "maithic.oracle.certificate/1",
  "kind": "linear.combination",
  "claim": "∀ x y : ℚ, 2 * x + 3 * y ≤ 6 → x ≥ 0 → y ≥ 0 → x + y ≤ 3",
  "data": { ... },
  "engine": {"name": "MaithicOracle", "version": "0.1.0", "parameters": {}}
}
```

`kind` fixes the shape of `data`. `claim` states the fact in Lean's notation, for people. `engine` records who
produced the certificate; no checker reads it. Numbers are exact and written as text (`"3"`, `"-7/2"`).

| Kind | `data` | The check |
|---|---|---|
| `linear.farkas` | `domain`, `variables`, `hypotheses`, `goal: "false"`, one refutation with `multipliers` | The weighted sum of the hypotheses has zero left side and a bound that makes it `0 ≤ b` with `b < 0`, or `0 < 0`. Multipliers of inequalities are not negative. |
| `linear.combination` | the same with a real goal; one refutation per case of the negated goal, each with its `assumption` | The same check per case. An equation `a = b` has the cases `a < b` and `a > b`. |
| `polynomial.identity` | `domain`, `variables`, `goal`, no hypotheses | Both sides have the same normal form. |
| `polynomial.combination` | the same with `hypotheses` and one cofactor per hypothesis | The goal's `left − right` equals the sum of cofactor times `left − right` of each hypothesis. |
| `interval.boxes` | `variables`, `hypotheses`, `goal`, `domain`, `boxes` with `bounds`, `reason`, `enclosure` | The boxes tile the domain by halving. On each, the enclosure contains the recomputed range and decides the goal (`reason` 0) or excludes hypothesis `reason`. |
| `discrete.prime` | `n`, `proof`: Pratt lines `{n, witness, factors}` | Each line: the factors multiply to `n − 1` and are proved earlier, `witness^(n−1) = 1` and `witness^((n−1)/q) ≠ 1` modulo `n` for each prime factor `q`. |
| `discrete.factorization` | `n`, `factors`, `proof` | The prime powers multiply to `n`; the primes increase and are proved by the Pratt lines. |
| `discrete.inverse` | `a`, `modulus`, `inverse` | `a · inverse` leaves remainder 1. |
| `discrete.order` | `a`, `modulus`, `order`, `factors`, `proof` | `a^order = 1`, and `a^(order/q) ≠ 1` for each proved prime factor `q` of the order. |
| `discrete.logarithm` | `base`, `target`, `modulus`, `exponent` | `base^exponent` and `target` leave the same remainder. |
| `discrete.square_root` | `a`, `modulus`, `root` | `root² − a` is divisible by the modulus. |
| `discrete.bezout` | `a`, `b`, `gcd`, `x`, `y`, `a_cofactor`, `b_cofactor` | `a·x + b·y = gcd`, and `gcd` times each cofactor gives `a` and `b`. |
| `discrete.witness` | `domain`, `variables`, `conditions`, `values` | Every condition holds at the values, exactly. |

### Integers

Over the integers each row of a linear certificate is first *tightened*: its coefficients are scaled to coprime
integers, its bound is rounded to the integer it implies, and a strict inequality becomes a weak one, so
`2x + 4y < 7` becomes `x + 2y ≤ 3`. An equation that no integers can satisfy becomes the false row `0 ≤ −1`.
The multipliers refer to the tightened rows, and a checker applies the same rule. This lets an exact rational
method refute many integer goals; it is sound and not complete, and when it does not suffice the outcome is
`unknown`.

The Lean library has a verified checker for exactly this form. `lean_replay(certificate)` writes the claim as a
Lean theorem closed by `maithic_linear_int`, with the tightened rows, the multipliers scaled to whole numbers,
and the hypothesis each row comes from; rows with multiplier zero are left out. See
[the guide to the checkers](../guides/logic-checkers.md#linear-integer-arithmetic).

### Boxes

The boxes of an interval certificate come from halving: a region is either one box, or the midpoint of one of
its sides separates the boxes into two groups that tile the two halves. A checker verifies the tiling by that
rule, with exact endpoints, and needs no floating-point comparison to know that the domain is covered. An
enclosure is rounded outward to short fractions when that still decides the box, so that certificates stay
readable.

## Writing and reading from Julia

```julia
using MaithicCore, MaithicOracle

outcome = solve(read("goal.json", String))            # dispatches on the goal's kind
write("outcome.json", write_json(outcome_document(outcome); pretty = true))

certificate = read_certificate(read("certificate.json", String))
check(certificate)                                    # the fact, re-established from the data alone
```

`read_certificate` validates the shape of a document, not its claim: a well-formed certificate of something
false is read without complaint and then fails `check`. `certificate_document` runs `check` before writing and
refuses a certificate that fails.
