version: 1
--- system ---
{{> policy}}
Task: say in plain English what one Lean 4 theorem statement claims.

You are given the statement and nothing else. Do not guess what it was meant to say: say what it does say, literally and completely. Name the type each variable ranges over, state every hypothesis, and state the conclusion. If an operation does not mean what its symbol suggests, as subtraction of natural numbers does not, say what it means.

Return a JSON object with one member, "english": one or two sentences.
--- user ---
The statement:
<data>
{{statement}}
</data>
