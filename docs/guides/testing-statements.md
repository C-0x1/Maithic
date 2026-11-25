# Testing a statement before proving it

A proof attempt on a false statement wastes its whole budget, and a proof of a vacuous statement proves nothing
worth having. Three commands of the Lean library put a statement to the test first. They need
`import Maithic.Refute.Testing` and `import Maithic.Refute.Vacuity`.

From the command line, [`maithic refute`](refuting-statements.md) runs these checks on the theorems of a file
and has Lean confirm every counterexample by a proof.

## `#maithic_test`: try to break it

```lean
#maithic_test ∀ l : List Nat, l.reverse.reverse = l
-- passed 100 cases

#maithic_test ∀ x y : Int, x * y ≥ 0
-- error: falsified at case 12, shrunk 2 times
--   x := 1
--   y := -1
```

The variables are generated, small at first and larger as the run goes on. A premise that fails discards its
case, so `∀ x : Nat, x > 5 → x * x > 30` is tested on values above five only, and the report says how many cases
were discarded. When a case fails, each value is shrunk for as long as the failure remains, so the
counterexample that is reported is small.

A falsified statement is an error: a file with a failing test does not compile. A statement that passes is not
proved, and the message does not say so.

The statement may be the name of a theorem, whose statement is then tested: `#maithic_test my_conjecture`.
With `json` after the command, the report is printed as JSON whatever the verdict, and a falsified statement is
not an error; this is the form a program reads.

```lean
#maithic_test json (cases := 20) (seed := 3) ∀ n : Nat, n + 0 = n
-- {"verdict": "passed", "shrinks": 0, "seed": 3, "discarded": 0, "counterexample": [], "cases": 20}
```

| Option | Meaning | Default |
|---|---|---|
| `(cases := N)` | cases that must pass | 100 |
| `(size := N)` | how large values get | 30 |
| `(seed := N)` | the seed of the random stream | 0 |
| `(discards := N)` | discarded cases after which the run gives up | 1000 |
| `(shrinks := N)` | shrinking steps per variable | 1000 |

A run is determined by its options. The stream is SplitMix64, the same as in the Julia packages, so a failure is
found again by running the same line, on any machine.

The tactic `maithic_test` does the same for the goal in front of it, with the hypotheses as premises:

```lean
example (x y : Nat) (h : x ≤ y) : x + 1 ≤ y := by
  maithic_test
  -- error: falsified at case 1
  --   x := 0
  --   y := 0
```

Types take part through three instances: `Arbitrary` to generate values, `Shrinkable` to simplify them and
`Repr` to print them. The library has them for naturals, integers, rationals, truth values, characters, strings,
`Fin`, lists, arrays, options and pairs.

## `#maithic_vacuous`: is it true for the wrong reasons?

```lean
#maithic_vacuous ∀ (x : Nat) (h₁ : x < 3) (h₂ : x > 5), x = 4
-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂)

#maithic_vacuous ∀ (i : Fin 0), i.val = 7
-- warning: vacuous: the variable i ranges over an empty type, Fin 0 (found by cases and omega)

#maithic_vacuous ∀ (x : Nat) (h : x > 3), x + 0 = x
-- warning: the conclusion holds without the hypotheses (found by rfl)
```

The command takes a statement or the name of a theorem. It looks for hypotheses that contradict each other, for
variables that range over empty types, and for a conclusion that holds whatever the hypotheses say.

Every finding rests on a proof that Lean's kernel accepted: a derivation of `False` from the hypotheses, a
proof that the type is empty, a proof of the conclusion from no hypothesis. A tactic that merely reports success
is not enough; the term it built is sent to the kernel as a closed theorem. The derivation names the hypotheses
it used. Finding nothing is only the absence of a proof within a budget; the message says "nothing found",
never "consistent". `#maithic_vacuous json` prints the findings as JSON.

The statements given to these commands name their hypotheses so that the findings can name them. Lean's linter
for unused variables is told to leave them alone.

## `#maithic_necessity`: which hypotheses are needed?

```lean
#maithic_necessity ∀ (a b : Nat) (h : a ≤ b) (h' : b < 1000), a ≤ b + 1
-- h : a ≤ b is needed: without it the statement fails at
--   a := 3
--   b := 0
-- h' : b < 1000 is not needed: the statement holds without it (found by omega)
```

Each hypothesis is dropped in turn. The statement without it is first tried by automation, and a proof settles
that the hypothesis can go. It is then tested by sampling, and a counterexample settles that it cannot. When
neither happens, the message says exactly that: the cases passed and no proof was found.

`#maithic_unused t` answers a narrower question about a theorem that is already proved: which hypotheses its
proof term does not mention.

## What a finding is worth

<p align="center">
  <img src="../../assets/readme/statement-checks.svg" alt="Three commands examine a statement. maithic_test generates values and reports that x times y at least zero is falsified at x equal to 1 and y equal to minus 1. maithic_vacuous reports that the hypotheses x less than 3 and x greater than 5 are contradictory, found by omega. maithic_necessity reports that the hypothesis a at most b is needed, with a counterexample, and that b less than 1000 is not. Below, the findings are sorted: vacuity and hypotheses that are not needed are accepted by the kernel, falsifications come with a counterexample, and passing cases or finding nothing is evidence only." width="900">
</p>

| Finding | Standing |
|---|---|
| vacuous, trivial conclusion, hypothesis not needed | the kernel accepted the proof behind it |
| falsified, hypothesis needed | a counterexample: values at which the statement was run and failed, small enough to check by hand |
| passed N cases | evidence only |
| nothing found | no evidence either way |

A counterexample is found by running the statement, not by a proof. Turning it into a theorem of Lean, the
negation of the statement, is planned; until then the values are printed so that they can be checked.
