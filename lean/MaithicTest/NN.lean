/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.NN.Cert.Property
import Maithic.NN.Examples.AbsoluteValue

/-!
# Tests of the network semantics and the certificate checkers

A small network with known behaviour, bounds that hold, bounds that do not, and a witness for the
ones that do not.
-/

namespace MaithicTest.NN

open Maithic.NN

/-- `|x|` as a ReLU network: `relu x + relu (-x)`. -/
def absNet : Network := [
  { neurons := [⟨[1], 0⟩, ⟨[-1], 0⟩], activation := .relu },
  { neurons := [⟨[1, 1], 0⟩], activation := .identity }]

/-- A layer with two inputs and biases: `(2x - y + 1/2, x + y - 1)`. -/
def affine : Layer :=
  { neurons := [⟨[2, -1], 1/2⟩, ⟨[1, 1], -1⟩], activation := .identity }

-- cap: P08-01-01
#guard affine.eval [3, 4] == [5/2, 6]
-- cap: P08-01-01
#guard (Neuron.eval ⟨[2, -1], 1/2⟩ [3, 4]) == 5/2

-- cap: P08-01-02
#guard relu (-3/2) == 0
-- cap: P08-01-02
#guard relu (7/3) == 7/3
-- cap: P08-01-02
example (x : Rat) : 0 ≤ relu x ∧ relu (relu x) = relu x := ⟨relu_nonneg x, relu_idem x⟩

-- cap: P08-01-03
#guard Network.eval absNet [-5/2] == [5/2]
-- cap: P08-01-03
#guard Network.eval absNet [3] == [3]
-- cap: P08-01-03
#guard Network.eval absNet [0] == [0]
-- cap: P08-01-03
#guard Network.eval [] [1, 2] == [1, 2]

-- cap: P08-02-01
#guard Interval.add ⟨1, 2⟩ ⟨-1, 5⟩ == ⟨0, 7⟩
-- cap: P08-02-01
#guard Interval.sub ⟨1, 2⟩ ⟨-1, 5⟩ == ⟨-4, 3⟩
-- cap: P08-02-01
#guard Interval.scale (-2) ⟨1, 3⟩ == ⟨-6, -2⟩
-- cap: P08-02-01
#guard Interval.scale (1/2) ⟨1, 3⟩ == ⟨1/2, 3/2⟩
-- cap: P08-02-01
#guard Interval.relu ⟨-1, 3⟩ == ⟨0, 3⟩
-- cap: P08-02-01
#guard Interval.hull ⟨0, 1⟩ ⟨3, 4⟩ == ⟨0, 4⟩
-- cap: P08-02-01
#guard decide ((1/2 : Rat) ∈ (⟨0, 1⟩ : Interval)) && !decide ((2 : Rat) ∈ (⟨0, 1⟩ : Interval))

-- cap: P08-02-02
example (i j : Interval) (x y c : Rat) (hx : x ∈ i) (hy : y ∈ j) :
    c * x + y ∈ Interval.add (Interval.scale c i) j :=
  Interval.add_sound (Interval.scale_sound c hx) hy

-- cap: P08-02-03
-- Propagating [-2, 3] through the absolute-value network gives [0, 5]: sound, and not tight
-- (the true range is [0, 3]), as expected of interval propagation.
#guard Network.bound absNet [⟨-2, 3⟩] == [⟨0, 5⟩]
-- cap: P08-02-03
#guard checkBounds absNet [⟨-2, 3⟩] [⟨0, 5⟩]
-- cap: P08-02-03
#guard checkBounds absNet [⟨-2, 3⟩] [⟨-1, 6⟩]
-- cap: P08-02-03
#guard !checkBounds absNet [⟨-2, 3⟩] [⟨0, 4⟩]
-- cap: P08-02-03
#guard !checkBounds absNet [⟨-2, 3⟩] [⟨0, 5⟩, ⟨0, 5⟩]

-- cap: P08-02-04
/-- A certificate with one box per layer, rounded outwards. -/
def absCert : Certificate := [[⟨0, 3⟩, ⟨0, 2⟩], [⟨0, 11/2⟩]]
-- cap: P08-02-04
#guard checkCertificate absNet [⟨-2, 3⟩] absCert
-- cap: P08-02-04
#guard finalBox [⟨-2, 3⟩] absCert == [⟨0, 11/2⟩]
-- cap: P08-02-04
-- A certificate whose first layer is too tight is rejected.
#guard !checkCertificate absNet [⟨-2, 3⟩] [[⟨0, 2⟩, ⟨0, 2⟩], [⟨0, 11/2⟩]]
-- cap: P08-02-04
-- A certificate with the wrong number of layers is rejected.
#guard !checkCertificate absNet [⟨-2, 3⟩] [[⟨0, 3⟩, ⟨0, 2⟩]]

-- cap: P08-02-05
example (h : checkBounds absNet [⟨-2, 3⟩] [⟨0, 5⟩] = true) (x : Rat) (hx : [x] ∈ ([⟨-2, 3⟩] : Box)) :
    Network.eval absNet [x] ∈ ([⟨0, 5⟩] : Box) :=
  checkBounds_sound h [x] hx

-- cap: P08-02-06
-- The claim "outputs stay below 2" is false on [-2, 3]; x = 5/2 is a witness.
#guard isCounterexample absNet [⟨-2, 3⟩] [⟨0, 2⟩] [5/2]
-- cap: P08-02-06
#guard !isCounterexample absNet [⟨-2, 3⟩] [⟨0, 2⟩] [1]
-- cap: P08-02-06
-- A point outside the input box is not a witness, whatever the network does there.
#guard !isCounterexample absNet [⟨-2, 3⟩] [⟨0, 2⟩] [10]

-- cap: P08-04-02
-- The property checker: the boxes must be accepted, and the last one must lie inside the claim.
#guard checkProperty absNet [⟨-2, 3⟩] [⟨0, 6⟩] absCert
-- cap: P08-04-02
#guard !checkProperty absNet [⟨-2, 3⟩] [⟨0, 5⟩] absCert
-- cap: P08-04-02
-- What propagation computes is a certificate the checker accepts, here with the sharper bound.
#guard propagated absNet [⟨-2, 3⟩] == [[⟨0, 3⟩, ⟨0, 2⟩], [⟨0, 5⟩]]
#guard checkProperty absNet [⟨-2, 3⟩] [⟨0, 5⟩] (propagated absNet [⟨-2, 3⟩])

-- cap: P08-04-02
-- The theorem `maithic nnv verify` writes for a property that holds: the kernel runs the checker.
example : Holds absNet [⟨-2, 3⟩] [⟨0, 6⟩] := checkProperty_sound (cert := absCert) (by decide +kernel)

-- cap: P08-04-02
-- And the one it writes for a property that fails.
example : ¬ Holds absNet [⟨-2, 3⟩] [⟨0, 2⟩] := not_holds_of_counterexample (xs := [3]) (by decide +kernel)

-- cap: P08-04-02
-- A claim is checked by the checker that fits it.
#guard (Claim.verified absCert).accepted absNet [⟨-2, 3⟩] [⟨0, 6⟩]
#guard (Claim.violated [3]).accepted absNet [⟨-2, 3⟩] [⟨0, 2⟩]
#guard !(Claim.violated [1]).accepted absNet [⟨-2, 3⟩] [⟨0, 2⟩]

-- cap: P08-04-02
-- No property has both an accepted certificate and an accepted counterexample.
example (xs : List Rat) : isCounterexample absNet [⟨-2, 3⟩] [⟨0, 6⟩] xs = false :=
  not_counterexample_of_checkProperty (cert := absCert) (by decide +kernel)

-- cap: P08-04-02
-- The example of the guide as theorems: the three verdicts of the verifier, and the fact behind
-- the third. The claim [0, 4] is true, and the checker does not accept it.
example : Holds Examples.absNet Examples.absInput [⟨0, 6⟩] := Examples.within_six
example : ¬ Holds Examples.absNet Examples.absInput [⟨0, 2⟩] := Examples.not_within_two
example : Holds Examples.absNet Examples.absInput [⟨0, 4⟩]
    ∧ checkProperty Examples.absNet Examples.absInput [⟨0, 4⟩]
        (propagated Examples.absNet Examples.absInput) = false :=
  ⟨Examples.within_four, Examples.checker_rejects_four⟩
example : Holds Examples.absNet Examples.absInput [⟨0, 3⟩] := Examples.within_three

-- cap: P08-01-03
-- The network is the absolute value at fifty-one points from -2 to 3.
#guard (List.range 51).all fun i =>
  let x : Rat := (i : Rat) / 10 - 2
  Network.eval Examples.absNet [x] == [if 0 ≤ x then x else -x]
example (x : Rat) : Network.eval Examples.absNet [x] = [relu x + relu (-x)] := Examples.absNet_eval x

end MaithicTest.NN
