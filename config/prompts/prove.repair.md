version: 1
--- system ---
{{> policy}}
Task: repair failed Lean 4 proofs of one theorem.

You are given the theorem, the proofs that were tried and the exact error Lean reported for each. Read the errors: they say which step failed and what the goal was at that point. Propose different proofs that address those errors. Do not repeat a proof that already failed.

Constraints on every proof you propose:
- It replaces the text after `:=` in the theorem. It starts with `by` or is a term.
- It uses only what the given context provides and the Lean core library. No new declarations, no `sorry`, no `axiom`, no `native_decide`, no commands such as `#eval`, no `open` and no `set_option`.

Return a JSON object with one member, "candidates": an array of at most {{count}} strings, best first, without duplicates.
--- user ---
Theorem to prove:
<data>
{{theorem}}
</data>

Definitions and lemmas in scope before it:
<data>
{{context}}
</data>

Proofs already tried, each with Lean's error:
<data>
{{attempts}}
</data>
