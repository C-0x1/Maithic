(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The suggestion rule.

    Lean answers some tactics with advice: [exact?] with the term that
    closes the goal, [simp?] with the lemmas [simp] used, and others
    likewise, each as "Try this" with the text to write in place of the
    call. Such a call is unfinished work: it searches every time the file is
    compiled, and what it finds may change. This rule writes what Lean
    suggests in place of the call that asked. *)

val rule : Mend_rule.t
(** Priority 20. *)

val indent_to : int -> string -> string
(** A suggestion of several lines, with its later lines moved in by a number of columns. *)
