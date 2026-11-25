/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Testing.Random

/-!
# Generating and shrinking values

Two classes say how a type takes part in testing. `Arbitrary α` generates values of `α`, larger
as the size grows. `Shrinkable α` lists, for a value, the values that are simpler than it; when a
test fails, the failing value is replaced by simpler ones for as long as the test still fails, so
that the counterexample reported is small. Shrinking draws nothing from the random stream: the
same failure always shrinks to the same counterexample.

Instances are given for naturals, integers, rationals, truth values, characters, strings, bounded
naturals, lists, arrays, options and pairs.
-/

namespace Maithic.Refute

/-- A type with a generator of random values. -/
class Arbitrary (α : Type) where
  /-- A random value, larger as the size grows. -/
  arbitrary : Gen α

/-- A type whose values can be made simpler. -/
class Shrinkable (α : Type) where
  /-- The values simpler than the given one that are tried in its place, the simplest first. The
  list is empty for a value that cannot be made simpler. -/
  shrink : α → List α

export Arbitrary (arbitrary)
export Shrinkable (shrink)

/-- The list without repeated values; of equal values the first is kept. -/
def distinct [BEq α] (values : List α) : List α :=
  values.foldl (fun seen v => if seen.contains v then seen else seen ++ [v]) []

/-! ## Numbers and truth values -/

instance : Arbitrary Nat := ⟨do Gen.chooseNat 0 (← Gen.size)⟩

instance : Shrinkable Nat := ⟨fun n => distinct ([0, n / 2, n - 1].filter (· < n))⟩

instance : Arbitrary Int := ⟨do
  let size ← Gen.size
  Gen.chooseInt (-size) size⟩

/-- An integer is simpler when it is closer to zero, or its own positive counterpart. -/
instance : Shrinkable Int := ⟨fun i =>
  if i = 0 then []
  else distinct (([0] ++ (if i < 0 then [-i] else []) ++ [i.tdiv 2, i - i.sign]).filter (· ≠ i))⟩

instance : Arbitrary Bool := ⟨Gen.bool⟩

instance : Shrinkable Bool := ⟨fun b => if b then [false] else []⟩

instance : Arbitrary Rat := ⟨do
  let size ← Gen.size
  let numerator ← Gen.chooseInt (-size) size
  let denominator ← Gen.chooseNat 1 (size + 1)
  pure (mkRat numerator denominator)⟩

/-- A rational is simpler when it is zero, a whole number, or a whole number closer to zero. -/
instance : Shrinkable Rat := ⟨fun q =>
  if q = 0 then []
  else
    let whole : Int := q.num.tdiv q.den
    distinct (([(0 : Rat), (whole : Rat)] ++ (if q.den = 1 then (shrink q.num).map (fun i : Int => (i : Rat)) else [])).filter
      (· ≠ q))⟩

instance {n : Nat} : Arbitrary (Fin (n + 1)) := ⟨do
  let size ← Gen.size
  let k ← Gen.below (min n size + 1)
  pure (Fin.ofNat (n + 1) k)⟩

instance {n : Nat} : Shrinkable (Fin (n + 1)) := ⟨fun i => (shrink i.val).map (Fin.ofNat (n + 1))⟩

/-! ## Characters and strings -/

/-- Letters, digits and a few characters that tend to expose mistakes. -/
def sampleChars : List Char :=
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 _-.,;:!?()\"'\\/\n\tλ∀é".toList

instance : Arbitrary Char := ⟨Gen.elements 'a' sampleChars⟩

instance : Shrinkable Char := ⟨fun c => if c = 'a' then [] else ['a']⟩

/-! ## Containers -/

instance [Arbitrary α] : Arbitrary (List α) := ⟨Gen.listOf arbitrary⟩

/-- The lists made by replacing one element with each of its simpler values. -/
def shrinkOneElement [Shrinkable α] : List α → List (List α)
  | [] => []
  | x :: xs => (shrink x).map (· :: xs) ++ (shrinkOneElement xs).map (x :: ·)

/-- The lists made by removing one element. -/
def removeOne : List α → List (List α)
  | [] => []
  | x :: xs => xs :: (removeOne xs).map (x :: ·)

/-- A list is made simpler by emptying it, halving it, removing one element, or making one
element simpler, in that order. -/
instance [Shrinkable α] : Shrinkable (List α) := ⟨fun l =>
  match l with
  | [] => []
  | [_] => [[]] ++ shrinkOneElement l
  | _ => [[], l.take (l.length / 2), l.drop (l.length / 2)] ++ removeOne l ++ shrinkOneElement l⟩

instance [Arbitrary α] : Arbitrary (Array α) := ⟨List.toArray <$> Gen.listOf arbitrary⟩

instance [Shrinkable α] : Shrinkable (Array α) := ⟨fun a => (shrink a.toList).map List.toArray⟩

instance : Arbitrary String := ⟨String.ofList <$> Gen.listOf arbitrary⟩

instance : Shrinkable String := ⟨fun s => (shrink s.toList).map String.ofList⟩

instance [Arbitrary α] : Arbitrary (Option α) := ⟨do
  if (← Gen.below 4) = 0 then pure none else some <$> arbitrary⟩

instance [Shrinkable α] : Shrinkable (Option α) := ⟨fun
  | none => []
  | some x => none :: (shrink x).map some⟩

instance [Arbitrary α] [Arbitrary β] : Arbitrary (α × β) := ⟨do
  let a ← arbitrary
  let b ← arbitrary
  pure (a, b)⟩

instance [Shrinkable α] [Shrinkable β] : Shrinkable (α × β) := ⟨fun (a, b) =>
  (shrink a).map (·, b) ++ (shrink b).map (a, ·)⟩

end Maithic.Refute
