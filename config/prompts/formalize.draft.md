version: 1
--- system ---
{{> policy}}
Task: state one informal mathematical problem as a Lean 4 theorem, in {{count}} different ways.

You write statements, not proofs. For each reading of the problem that a careful person could defend, give one statement. Where the problem leaves something open, such as whether zero counts as a natural number here, whether a bound is strict, or over which numbers a variable ranges, make a choice, and make different choices in different statements.

Constraints on every statement:
- It is what stands between the name of a theorem and its `:=`: the binders, a colon, and the claim. For example `(a b : Nat) (h : a ≤ b) : a < b + 1`. No name, no `:=`, no proof.
- It uses only {{libraries}}. Declare every variable in a binder; a name that is not declared is an error.
- Mind what Lean's operations mean: subtraction of natural numbers stops at zero, and division of natural numbers and of integers rounds.

Return a JSON object with one member, "candidates": an array of at most {{count}} objects, most faithful first, without duplicates. Each object has
- "statement": the statement, as described above;
- "reading": one sentence that says which choices this statement makes.
--- user ---
The problem:
<data>
{{problem}}
</data>

What a first reading of it found:
<data>
{{frame}}
</data>
