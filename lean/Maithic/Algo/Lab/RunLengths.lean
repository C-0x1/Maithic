/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# From tested to proved: run-length encoding

`runLengths` below was not written for this library. It is the function that `maithic algo synth`
accepted for the task `examples/algo/run-lengths.toml`, copied as it was written: a model drafted
it, Lean compiled it, and five examples gave what was expected. That is a test. Five examples say
nothing about the sixth.

This file takes the next step. It says what a run-length encoding has to be, and proves that the
function is one:

* `expand_runLengths`: decoding the result gives back the list, so nothing is lost and nothing is
  added;
* `runLengths_pos`: no run has length zero;
* `separated_runLengths`: two neighbouring runs have different values, so every run is maximal;
* `sum_runLengths`: the lengths of the runs add up to the length of the list.

Four consequences follow at once: there are never more runs than elements
(`length_runLengths_le`), only the empty list has none (`runLengths_eq_nil`), one value repeated
is one run (`runLengths_replicate`), and two lists with the same runs are the same list
(`runLengths_injective`).

The first three determine the encoding: `eq_runLengths` proves that any list of pairs with those
three properties is the one `runLengths` returns. So the specification has exactly one solution,
and the function that a model proposed is it.
-/

namespace Maithic.Algo.Lab

/-- The maximal runs of equal neighbours in `l`, in order, each as its value and its length; the
empty list has no runs. -/
def runLengths (l : List Nat) : List (Nat × Nat) :=
  match l with
  | [] => []
  | x :: rest =>
    match runLengths rest with
    | (y, n) :: tail => if x = y then (y, n + 1) :: tail else (x, 1) :: (y, n) :: tail
    | [] => [(x, 1)]

/-- Decoding: every pair of a value and a length becomes that many copies of the value. -/
def expand : List (Nat × Nat) → List Nat
  | [] => []
  | (x, n) :: rest => List.replicate n x ++ expand rest

/-- Neighbouring runs have different values. -/
def Separated : List (Nat × Nat) → Prop
  | (x, _) :: (y, m) :: rest => x ≠ y ∧ Separated ((y, m) :: rest)
  | _ => True

@[simp] theorem runLengths_nil : runLengths [] = [] := rfl

/-- One more element in front: it joins the first run when it has the value of that run, and
starts a run of its own when it does not. -/
theorem runLengths_cons (x : Nat) (rest : List Nat) :
    runLengths (x :: rest) =
      match runLengths rest with
      | (y, n) :: tail => if x = y then (y, n + 1) :: tail else (x, 1) :: (y, n) :: tail
      | [] => [(x, 1)] := by
  rw [runLengths]

/-- Decoding the encoding gives the list back. -/
theorem expand_runLengths (l : List Nat) : expand (runLengths l) = l := by
  induction l with
  | nil => rfl
  | cons x rest ih =>
    rw [runLengths_cons]
    split
    · next y n tail h =>
      rw [h] at ih
      split
      · next hxy =>
        subst hxy
        simp only [expand, List.replicate_succ, List.cons_append] at ih ⊢
        rw [ih]
      · simp only [expand, List.replicate_succ, List.replicate_zero, List.cons_append, List.nil_append] at ih ⊢
        rw [ih]
    · next h =>
      rw [h] at ih
      simp only [expand, List.replicate_succ, List.replicate_zero, List.append_nil] at ih ⊢
      rw [← ih]

/-- No run is empty. -/
theorem runLengths_pos (l : List Nat) : ∀ p ∈ runLengths l, 0 < p.2 := by
  induction l with
  | nil => intro p hp; simp at hp
  | cons x rest ih =>
    rw [runLengths_cons]
    split
    · next y n tail h =>
      rw [h] at ih
      split
      · intro p hp
        rcases List.mem_cons.mp hp with rfl | hp
        · exact Nat.succ_pos n
        · exact ih p (List.mem_cons_of_mem _ hp)
      · intro p hp
        rcases List.mem_cons.mp hp with rfl | hp
        · exact Nat.one_pos
        · exact ih p hp
    · intro p hp
      rcases List.mem_cons.mp hp with rfl | hp
      · exact Nat.one_pos
      · simp at hp

/-- Neighbouring runs have different values: no run could have been longer. -/
theorem separated_runLengths (l : List Nat) : Separated (runLengths l) := by
  induction l with
  | nil => trivial
  | cons x rest ih =>
    rw [runLengths_cons]
    split
    · next y n tail h =>
      rw [h] at ih
      split
      · cases tail with
        | nil => trivial
        | cons q tail' => exact ih
      · next hxy => exact ⟨hxy, ih⟩
    · trivial

/-- A decoded list is as long as its runs together. -/
theorem length_expand (ps : List (Nat × Nat)) : (expand ps).length = (ps.map (·.2)).sum := by
  induction ps with
  | nil => rfl
  | cons p rest ih =>
    obtain ⟨x, n⟩ := p
    simp only [expand, List.length_append, List.length_replicate, List.map_cons, List.sum_cons, ih]

/-- The lengths of the runs add up to the length of the list. -/
theorem sum_runLengths (l : List Nat) : ((runLengths l).map (·.2)).sum = l.length := by
  rw [← length_expand, expand_runLengths]

/-- The encoding of a decoded list is the list of runs it was decoded from, when those runs are
not empty and neighbours differ. -/
theorem runLengths_expand : ∀ (ps : List (Nat × Nat)), (∀ p ∈ ps, 0 < p.2) → Separated ps →
    runLengths (expand ps) = ps
  | [], _, _ => rfl
  | (x, n) :: rest, hpos, hsep => by
    have hrest : runLengths (expand rest) = rest :=
      runLengths_expand rest (fun p hp => hpos p (List.mem_cons_of_mem _ hp))
        (by cases rest with
            | nil => trivial
            | cons q rest' => exact hsep.2)
    have hn : 0 < n := hpos (x, n) (List.mem_cons_self ..)
    -- one copy of `x` in front of the encoded rest, then the others one at a time
    have one : runLengths (x :: expand rest) = (x, 1) :: rest := by
      rw [runLengths_cons, hrest]
      cases rest with
      | nil => rfl
      | cons q rest' =>
        obtain ⟨y, m⟩ := q
        have hxy : x ≠ y := hsep.1
        simp [hxy]
    have more : ∀ k, runLengths (List.replicate (k + 1) x ++ expand rest) = (x, k + 1) :: rest := by
      intro k
      induction k with
      | zero => simpa using one
      | succ k ih =>
        rw [List.replicate_succ, List.cons_append, runLengths_cons, ih]
        simp
    obtain ⟨k, rfl⟩ : ∃ k, n = k + 1 := ⟨n - 1, by omega⟩
    exact more k

/-- The specification has one solution: a list of runs that decodes to `l`, has no empty run and
no two neighbours of the same value, is the list `runLengths l`. -/
theorem eq_runLengths {l : List Nat} {ps : List (Nat × Nat)} (hexpand : expand ps = l)
    (hpos : ∀ p ∈ ps, 0 < p.2) (hsep : Separated ps) : ps = runLengths l := by
  rw [← hexpand, runLengths_expand ps hpos hsep]

/-- Runs that are not empty are no more in number than the elements they hold. -/
theorem length_le_sum_of_pos : ∀ (ps : List (Nat × Nat)), (∀ p ∈ ps, 0 < p.2) →
    ps.length ≤ (ps.map (·.2)).sum
  | [], _ => Nat.le_refl 0
  | p :: rest, h => by
    have hp : 0 < p.2 := h p (List.mem_cons_self ..)
    have ih := length_le_sum_of_pos rest (fun q hq => h q (List.mem_cons_of_mem _ hq))
    simp only [List.length_cons, List.map_cons, List.sum_cons]
    omega

/-- There are never more runs than elements: the encoding of `n` numbers has at most `n` pairs. -/
theorem length_runLengths_le (l : List Nat) : (runLengths l).length ≤ l.length := by
  have h := length_le_sum_of_pos (runLengths l) (runLengths_pos l)
  rw [sum_runLengths] at h
  exact h

/-- Only the empty list has no runs. -/
theorem runLengths_eq_nil {l : List Nat} : runLengths l = [] ↔ l = [] := by
  constructor
  · intro h
    have hback := expand_runLengths l
    rw [h] at hback
    simpa [expand] using hback.symm
  · rintro rfl
    rfl

/-- One value repeated is one run. -/
theorem runLengths_replicate (x : Nat) {n : Nat} (h : 0 < n) :
    runLengths (List.replicate n x) = [(x, n)] := by
  have key := runLengths_expand [(x, n)] (by simpa using h) trivial
  simpa [expand] using key

/-- Two lists with the same runs are the same list: nothing is lost in the encoding. -/
theorem runLengths_injective {l m : List Nat} (h : runLengths l = runLengths m) : l = m := by
  rw [← expand_runLengths l, ← expand_runLengths m, h]

end Maithic.Algo.Lab
