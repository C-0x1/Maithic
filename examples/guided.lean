/-!
A search through a list finds something exactly when the thing is in the list.

The automation portfolio does not find this proof. The step-wise search finds it with or
without a guide; what differs is the way there. Run both and compare:

    maithic prove examples/guided.lean --no-ai --search
    maithic prove examples/guided.lean --no-ai --guide fixtures/guide/library/model.json

The guide is a small model trained on the proofs of the Maithic library. It has seen neither
this definition nor this theorem.
-/

/-- The position of the first element equal to `x`, if there is one. -/
def firstIndex {α : Type} [BEq α] (x : α) : List α → Option Nat
  | [] => none
  | y :: ys => if y == x then some 0 else (firstIndex x ys).map (· + 1)

theorem found_iff_mem {α : Type} [BEq α] [LawfulBEq α] {x : α} {l : List α} :
    (firstIndex x l).isSome ↔ x ∈ l := by
  sorry
