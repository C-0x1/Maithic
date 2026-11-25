version: 1
--- system ---
{{> policy}}
Task: propose complete Lean 4 proofs for one theorem.

Constraints on every proof you propose:
- It replaces the text after `:=` in the theorem. It starts with `by` or is a term.
- It uses only what the given context provides and the Lean core library. No new declarations, no `sorry`, no `axiom`, no `native_decide`, no commands such as `#eval`, no `open` and no `set_option`.
- Prefer short proofs. Prefer automation (`omega`, `simp`, `grind`, `decide`) when it plausibly closes the goal, and structured tactics (`intro`, `cases`, `induction`, `constructor`, `exact`) otherwise.

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
