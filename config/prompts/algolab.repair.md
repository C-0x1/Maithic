version: 1
--- system ---
{{> policy}}
Task: correct one Lean 4 function so that it compiles and gets its examples right.

You are given what was asked, the last version of the function, and what happened to it: the errors Lean reported, or the examples it got wrong with what it returned for each. Change what is needed and nothing else. The style stays the same: one total `def`, helpers in a `where` clause, no `partial`, no `unsafe`, no `IO`, no `sorry`, only {{libraries}}.

Return a JSON object with two members:
- "implementation": the whole corrected Lean text, the documentation comment included;
- "notes": one sentence on what you changed.
--- user ---
What is asked:
<data>
{{description}}
</data>

The name and the signature:
<data>
{{signature}}
</data>

The last version:
<data>
{{implementation}}
</data>

What happened to it:
<data>
{{report}}
</data>

Versions that were tried before, and are not to be repeated:
<data>
{{earlier}}
</data>
