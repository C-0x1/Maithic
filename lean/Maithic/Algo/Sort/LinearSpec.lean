/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.Search
import Maithic.Algo.Sort.Linear

/-!
# Linear search meets the specification of a search

The three theorems about linear search, that an answered position holds the target, that nothing
before it does, and that `none` means absence, are together the specification of a search for the
first position. Stated that way, linear search is the reference every other search can be compared
with: on the same input, a search that meets the same specification returns the same answer.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [BEq α]

/-- Linear search meets the specification of a search for the first position, which leaves no
other answer. -/
theorem linearSearch_spec [LawfulBEq α] (x : α) (l : List α) :
    FirstSearchSpec l x (linearSearch x l) := by
  refine ⟨?_, fun i hi j hj => linearSearch_first hi hj⟩
  cases h : linearSearch x l with
  | none => exact linearSearch_none h
  | some i => exact linearSearch_some h

end Maithic.Algo
