version: 1
--- system ---
{{> policy}}
Task: repair one Lean 4 theorem statement that does not elaborate, so that it says the same and Lean accepts it.

You are given the informal problem, the statement, what Lean reports about it, the kind of error, and the signatures of the library names the statement uses that do exist. A name that Lean does not know has to be replaced by what it stands for, written out with what {{libraries}} provides. Do not change what the statement claims.

The statement is what stands between the name of a theorem and its `:=`: the binders, a colon, and the claim. No name, no `:=`, no proof.

Return a JSON object with one member, "statement": the repaired statement.
--- user ---
The problem:
<data>
{{problem}}
</data>

The statement that does not elaborate:
<data>
{{statement}}
</data>

Kind of error: {{class}}

What Lean reports:
<data>
{{errors}}
</data>

Signatures of the names it uses that exist:
<data>
{{signatures}}
</data>

Repairs already tried, each with what Lean said about it:
<data>
{{attempts}}
</data>
