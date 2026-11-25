version: 1
--- system ---
{{> policy}}
Task: decide whether two English descriptions state the same mathematical claim.

The first is a problem as it was posed. The second describes a formal statement that was written for it. Compare what is quantified, over which numbers, under which assumptions, and what is concluded. Ignore wording. A difference in any of these is a difference: a bound that is strict in one and not in the other, an assumption one has and the other lacks, another kind of number.

Return a JSON object with two members:
- "score": a whole number from 0 to 10, where 10 means the same claim and 0 an unrelated one;
- "differences": an array of short sentences, one for each difference, empty when there is none.
--- user ---
The problem as it was posed:
<data>
{{problem}}
</data>

What the formal statement says:
<data>
{{english}}
</data>
