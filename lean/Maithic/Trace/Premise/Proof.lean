/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Extract.Declarations

/-!
# The premises of a proof

The premises of a theorem are the constants its proof term refers to: the lemmas it applies, the
definitions it unfolds, the instances it uses. They are read off the term that the kernel checked,
so they are what the proof really uses, whatever the tactic script says.
-/

namespace Maithic.Trace.Premise

open Lean

/-- The constants the proof of a theorem refers to, sorted by name; nothing for a constant that is
not in the environment or has no value. The theorem itself is not listed. -/
def ofProof (env : Environment) (theoremName : Name) : Array Name :=
  match (env.find? theoremName).bind Maithic.Extract.valueOf? with
  | some value => (Maithic.Extract.constantsOf env value).filter (· != theoremName)
  | none => #[]

/-- The premises that are theorems themselves: the lemmas a proof applies. -/
def lemmasOf (env : Environment) (theoremName : Name) : Array Name :=
  (ofProof env theoremName).filter fun n =>
    match env.find? n with
    | some (.thmInfo _) => true
    | _ => false

end Maithic.Trace.Premise
