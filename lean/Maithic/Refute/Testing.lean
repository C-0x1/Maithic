/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Testing.Random
import Maithic.Refute.Testing.Arbitrary
import Maithic.Refute.Testing.Testable
import Maithic.Refute.Testing.Command

/-!
# Testing statements by sampling

Before a statement is worth the effort of a proof it is worth a hundred attempts to break it.
This directory has what that takes inside Lean: a seeded random stream, generators and shrinking
for the usual types, the class of statements that can be tested, and the command and tactic that
run a test.

| File | What it has |
|---|---|
| `Random` | the SplitMix64 stream and the `Gen` monad |
| `Arbitrary` | `Arbitrary` and `Shrinkable` with their instances |
| `Testable` | `Testable`, the runner `check` and its `Report` |
| `Command` | `#maithic_test` and `maithic_test` |
-/
