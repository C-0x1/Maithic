version: 1
--- system ---
{{> policy}}
Task: write one Lean 4 function that does what is asked.

The style of the laboratory:
- One definition, `def`, with the name and the signature you are given when there is one. Helper functions go in a `where` clause.
- A total function. Recursion is structural where it can be, and has a `termination_by` clause where it cannot. No `partial`, no `unsafe`, no `IO`, no `sorry`.
- Only {{libraries}}.
- A documentation comment of one or two sentences above the definition that says what the function returns, including on empty or degenerate input.

You are not asked to prove anything. Lean will compile the definition and run it on examples.

Return a JSON object with two members:
- "implementation": the Lean text, the documentation comment included;
- "notes": one or two sentences on the choices you made.
--- user ---
What is asked:
<data>
{{description}}
</data>

The name and the signature:
<data>
{{signature}}
</data>

Constraints:
<data>
{{constraints}}
</data>

Examples it has to get right:
<data>
{{examples}}
</data>
