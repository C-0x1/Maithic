/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Random values that are the same everywhere

Testing by sampling is only useful when a failure can be found again. The generator here is
SplitMix64, whose stream is fixed by its definition: a seed gives the same values on every
machine and in every release, and the same values as the generator of the Julia packages, so a
counterexample reported with its seed can be reproduced anywhere.

`Gen α` is a computation that draws from the stream and reads a *size*, a number that bounds how
large the generated values may be. A test run starts with small sizes and lets them grow.
-/

namespace Maithic.Refute

/-- The state of a SplitMix64 generator. -/
structure Rng where
  /-- The 64 bits of state. -/
  state : UInt64
  deriving Repr, Inhabited, DecidableEq

namespace Rng

/-- The generator that a seed starts. Seeds are taken modulo `2 ^ 64`. -/
def ofSeed (seed : Nat) : Rng := ⟨UInt64.ofNat seed⟩

/-- The next 64 bits of the stream, and the generator after them. -/
def next (g : Rng) : UInt64 × Rng :=
  let s := g.state + 0x9e3779b97f4a7c15
  let z := (s ^^^ (s >>> 30)) * 0xbf58476d1ce4e5b9
  let z := (z ^^^ (z >>> 27)) * 0x94d049bb133111eb
  (z ^^^ (z >>> 31), ⟨s⟩)

/-- Draws until a value is small enough to be reduced without bias. `fuel` bounds the number of
draws; a draw is rejected with probability below one half, so the bound is never reached in
practice, and the last draw is used if it is. -/
def belowLoop (span limit : Nat) : Nat → Rng → Nat × Rng
  | 0, g => let (x, g) := g.next; (x.toNat % span, g)
  | fuel + 1, g =>
    let (x, g) := g.next
    if x.toNat ≤ limit then (x.toNat % span, g) else belowLoop span limit fuel g

/-- A number below `span`, uniform and without modulo bias, for `0 < span ≤ 2 ^ 64`. A span of
zero gives zero and draws nothing. -/
def below (span : Nat) (g : Rng) : Nat × Rng :=
  if span = 0 then (0, g)
  else
    let top := 2 ^ 64 - 1
    belowLoop span (top - (top % span + 1) % span) 64 g

end Rng

/-- A generator of random values: it reads a size and draws from the stream. -/
abbrev Gen (α : Type) := ReaderT Nat (StateM Rng) α

namespace Gen

/-- Runs a generator at a size, from a state. -/
def run (x : Gen α) (size : Nat) (g : Rng) : α × Rng := (ReaderT.run x size).run g

/-- The size the generator runs at. -/
def size : Gen Nat := read

/-- Runs a generator at another size. -/
def resize (f : Nat → Nat) (x : Gen α) : Gen α := withReader f x

/-- The next 64 bits of the stream. -/
def bits : Gen UInt64 := fun _ => modifyGet Rng.next

/-- A number below `span`, uniform. -/
def below (span : Nat) : Gen Nat := fun _ => modifyGet (Rng.below span)

/-- A natural number between `lo` and `hi`, both included; `lo` when the range is empty. -/
def chooseNat (lo hi : Nat) : Gen Nat := (lo + ·) <$> below (hi + 1 - lo)

/-- An integer between `lo` and `hi`, both included; `lo` when the range is empty. -/
def chooseInt (lo hi : Int) : Gen Int := (fun k : Nat => lo + k) <$> below (hi + 1 - lo).toNat

/-- A truth value. -/
def bool : Gen Bool := (· == 1) <$> below 2

/-- One of the listed values, each as likely as the others; `default` when the list is empty. -/
def elements (default : α) (values : List α) : Gen α :=
  (fun i => values.getD i default) <$> below values.length

/-- The result of one of the listed generators, each as likely as the others. -/
def oneOf (default : Gen α) (generators : List (Gen α)) : Gen α := do
  generators.getD (← below generators.length) default

/-- A list of the given length. -/
def listOfLength (x : Gen α) : Nat → Gen (List α)
  | 0 => pure []
  | n + 1 => do
    let head ← x
    let tail ← listOfLength x n
    pure (head :: tail)

/-- A list whose length is at most the size. -/
def listOf (x : Gen α) : Gen (List α) := do
  listOfLength x (← below ((← size) + 1))

end Gen

end Maithic.Refute
