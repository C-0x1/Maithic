/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBridge.Server.Guard

/-!
# Tests of the candidate guard

Ordinary proofs pass; anything that could run a program, add an axiom or change how later text is
read does not.
-/

namespace MaithicTest.Guard

open Maithic.Bridge.Guard

#guard admits "by omega"
#guard admits "by intro a b h; simp [Nat.add_comm] at h ⊢; exact h.symm"
#guard admits "fun h => h.elim Or.inr Or.inl"
#guard admits "by\n  induction xs with\n  | nil => rfl\n  | cons x xs ih => simp [ih]"
#guard admits "by exact?"
#guard admits "by have := Nat.lt_irrefl 0; decide"

#guard !admits "by sorry"
#guard !admits "by admit"
#guard !admits "by native_decide"
#guard !admits "by run_tac (pure ())"
#guard !admits "by first | omega | (#eval 1)"
#guard !admits "by exact IO.println \"x\""
#guard !admits "by exact _root_.IO.println \"x\""
#guard !admits "by exact _root_._root_.System.FilePath.mk \"x\""
#guard !admits "rfl\naxiom cheat : False"
#guard !admits "rfl\ntheorem other : True := trivial"
#guard !admits "by set_option maxHeartbeats 0 in simp"
#guard !admits "by open Classical in exact em _"
#guard !admits "by exact «sorryAx» _"
#guard !admits "by exact Lean.Elab.Command.elabCommand"
#guard !admits (String.ofList (List.replicate (maxCandidateChars + 1) 'a'))

#guard violation "by sorry" == some "forbidden keyword 'sorry'"
#guard violation "by exact IO.println 1" == some "forbidden name 'IO.println' (matches 'IO.')"
#guard violation "by omega" == none

#guard words "by simp [foo.bar] at h ⊢" == ["by", "simp", "foo.bar", "at", "h"]
#guard stripRoot "_root_._root_.IO.print" == "IO.print"

end MaithicTest.Guard
