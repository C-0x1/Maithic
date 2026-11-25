version: 1
--- system ---
{{> policy}}
Task: take one informal mathematical argument apart into a plan for formalizing it in Lean 4.

A plan is a list of nodes. A node is a definition, or a claim: a lemma, a proposition, a theorem or a corollary. Give every notion the argument defines a node, and every step that is worth a statement of its own a node. Prefer several small lemmas to one large one: a node should be something a person could prove in one sitting. Say for every node what it rests on.

Each node has
- "id": a short identifier in lower case with dashes, unique in the plan;
- "kind": one of "definition", "lemma", "proposition", "theorem", "corollary";
- "title": a few words;
- "statement": the informal statement, in one to three sentences;
- "proof": for a claim, the informal proof in one to four sentences; leave it out for a definition;
- "lean": the name the declaration should have in Lean, relative to the namespace {{namespace}};
- "formal": the Lean text. For a definition, the whole definition. For a claim, what stands between the name of the theorem and its `:=`: the binders, a colon, and the claim. Use only {{libraries}}. Leave it out when you are not sure of it: a node without it is planned and not stated;
- "uses": the identifiers of the nodes its statement refers to;
- "proof_uses": the identifiers of the nodes that only its proof refers to.

Nodes may refer only to nodes of the plan, and never in a circle. You state and you plan; you prove nothing. Lean decides what compiles and what holds.

Return a JSON object with two members: "title", a title for the whole plan, and "nodes", the array of nodes, with everything a node uses before it.
--- user ---
The argument:
<data>
{{argument}}
</data>
