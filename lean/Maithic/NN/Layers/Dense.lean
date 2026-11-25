/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Exact semantics of feed-forward networks

The reference meaning of a ReLU network, over exact rational numbers. Nothing here rounds: this
is the function that every certificate checker in `Maithic.NN.Cert` is proved sound against.

A network is a list of layers. A layer is a list of neurons, each a weight row with a bias, and
an activation applied to every output of the layer. Vectors are lists; a row shorter or longer
than its input uses the common prefix, and the checkers use the same convention, so soundness
does not depend on shapes being consistent.
-/

namespace Maithic.NN

/-- The rectified linear unit. -/
def relu (x : Rat) : Rat := max 0 x

theorem relu_nonneg (x : Rat) : 0 ≤ relu x := by
  unfold relu; grind

theorem relu_of_nonneg {x : Rat} (h : 0 ≤ x) : relu x = x := by
  unfold relu; grind

theorem relu_of_nonpos {x : Rat} (h : x ≤ 0) : relu x = 0 := by
  unfold relu; grind

theorem relu_mono {x y : Rat} (h : x ≤ y) : relu x ≤ relu y := by
  unfold relu; grind

theorem relu_idem (x : Rat) : relu (relu x) = relu x := by
  unfold relu; grind

theorem le_relu (x : Rat) : x ≤ relu x := by
  unfold relu; grind

/-- The activation applied after a layer. -/
inductive Activation where
  | identity
  | relu
  deriving Repr, BEq, DecidableEq, Inhabited

/-- Applies an activation to one number. -/
def Activation.apply : Activation → Rat → Rat
  | .identity, x => x
  | .relu, x => NN.relu x

/-- The dot product of the common prefix of two vectors. -/
def dot : List Rat → List Rat → Rat
  | w :: ws, x :: xs => w * x + dot ws xs
  | _, _ => 0

/-- One neuron: a weight row and a bias. -/
structure Neuron where
  weights : List Rat
  bias : Rat
  deriving Repr, BEq, Inhabited

/-- The affine value of a neuron before the activation. -/
def Neuron.eval (n : Neuron) (xs : List Rat) : Rat := dot n.weights xs + n.bias

/-- One layer: its neurons and the activation applied to each output. -/
structure Layer where
  neurons : List Neuron
  activation : Activation
  deriving Repr, BEq, Inhabited

/-- The output vector of a layer. -/
def Layer.eval (l : Layer) (xs : List Rat) : List Rat :=
  l.neurons.map fun n => l.activation.apply (n.eval xs)

/-- A network is a list of layers, applied first to last. -/
abbrev Network := List Layer

/-- The output vector of a network. -/
def Network.eval : Network → List Rat → List Rat
  | [], xs => xs
  | l :: ls, xs => Network.eval ls (l.eval xs)

@[simp] theorem Network.eval_nil (xs : List Rat) : Network.eval [] xs = xs := rfl

@[simp] theorem Network.eval_cons (l : Layer) (ls : Network) (xs : List Rat) :
    Network.eval (l :: ls) xs = Network.eval ls (l.eval xs) := rfl

/-- Evaluating two networks in sequence is evaluating their concatenation. -/
theorem Network.eval_append (n₁ n₂ : Network) (xs : List Rat) :
    Network.eval (n₁ ++ n₂) xs = Network.eval n₂ (Network.eval n₁ xs) := by
  induction n₁ generalizing xs with
  | nil => rfl
  | cons l ls ih => simp [ih]

/-- A layer produces one output per neuron. -/
theorem Layer.length_eval (l : Layer) (xs : List Rat) : (l.eval xs).length = l.neurons.length := by
  simp [Layer.eval]

end Maithic.NN
