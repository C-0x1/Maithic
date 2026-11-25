(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The blueprint file format: Markdown with front matter and one node for
    each section of the second level.

    {v
    ---
    blueprint: 1
    title: Sums of odd numbers
    namespace: SumOdd
    ---

    ## Lemma `step`: Adding the next odd number

    - lean: `sumOdd_succ`
    - uses: `sum-odd`

    The informal statement.

    ```lean
    (n : Nat) : sumOdd (n + 1) = sumOdd n + (2 * n + 1)
    ```

    **Proof.** The informal proof.
    v} *)

val version : int

val proof_marker : string
(** What opens the informal proof of a node. *)

val parse : string -> (Blueprint_doc.t, string) result
(** Reads a blueprint. An error names the line. Nothing is checked beyond the
    form: {!Blueprint_check.validate} looks at what the nodes say. *)

val print : Blueprint_doc.t -> string
(** Writes a blueprint in its canonical form. [parse (print doc)] is [doc]. *)

val print_node : Blueprint_node.t -> string
val read : string -> (Blueprint_doc.t, string) result
