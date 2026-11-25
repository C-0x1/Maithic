/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Audit.Axioms

/-!
# Tests of the axiom audit

Positive and negative fixtures for every rule. The one axiom declared here is a test fixture: it
states `True`, and exists so that the "non-standard axiom" rule has something to find.
-/

namespace MaithicTest.Audit

open Lean Maithic.Audit

theorem noAxioms : 2 + 2 = 4 := rfl

theorem standardOnly (a b : Nat) (h : a % 2 = 0) (h' : b % 2 = 0) : (a + b) % 2 = 0 := by omega

/-- Test fixture for the non-standard axiom rule. It asserts nothing: its statement is `True`. -/
axiom fixtureAxiom : True

theorem usesFixture : True := fixtureAxiom

theorem usesFixtureIndirectly : True ∧ True := ⟨usesFixture, trivial⟩

-- cap: P18-01-01
/-- info: MaithicTest.Audit.noAxioms: clean (no axioms) -/
#guard_msgs in
#maithic_audit noAxioms

-- cap: P18-01-04
/-- info: MaithicTest.Audit.standardOnly: clean (standard axioms: propext, Quot.sound) -/
#guard_msgs in
#maithic_audit standardOnly

-- cap: P18-01-01
-- Collection is transitive: the axiom is found through an intermediate theorem.
/-- info: MaithicTest.Audit.usesFixtureIndirectly: depends on non-standard axioms (MaithicTest.Audit.fixtureAxiom) -/
#guard_msgs in
#maithic_audit usesFixtureIndirectly

-- cap: P18-01-03
#guard classify ``propext == .standard
-- cap: P18-01-03
#guard classify ``Quot.sound == .standard
-- cap: P18-01-03
#guard classify ``Classical.choice == .standard
-- cap: P18-01-03
#guard classify ``fixtureAxiom == .other
-- cap: P18-01-02
#guard classify ``sorryAx == .incomplete
-- cap: P18-01-05
#guard classify `foo._native.native_decide.ax_1_1 == .native
-- cap: P18-01-05
#guard classify `Lean.ofReduceBool == .native

-- cap: P18-01-06
#guard (Result.ofAxioms `t #[]).verdict == .clean
-- cap: P18-01-06
#guard (Result.ofAxioms `t #[``propext, ``Classical.choice]).verdict == .clean
-- cap: P18-01-02
#guard (Result.ofAxioms `t #[``propext, ``sorryAx]).verdict == .usesSorry
-- cap: P18-01-02
#guard (Result.ofAxioms `t #[``sorryAx, ``fixtureAxiom]).usesSorry
-- cap: P18-01-05
#guard (Result.ofAxioms `t #[`t._native.native_decide.ax_1_1, ``propext]).verdict == .usesNative
-- cap: P18-01-06
#guard (Result.ofAxioms `t #[``fixtureAxiom, `t._native.decide.ax_1_1]).verdict == .nonStandard
-- cap: P18-01-06
-- Axioms are listed in the order Lean itself prints them.
#guard (Result.ofAxioms `t #[``Quot.sound, ``propext]).axioms == #[``propext, ``Quot.sound]
-- cap: P18-01-06
#guard (Result.ofAxioms `t #[``fixtureAxiom]).toJson.compress ==
  "{\"axioms\":[\"MaithicTest.Audit.fixtureAxiom\"],\"name\":\"t\",\"native\":[],\"other\":[\"MaithicTest.Audit.fixtureAxiom\"],\"standard\":[],\"usesSorry\":false,\"verdict\":\"non-standard-axioms\"}"

end MaithicTest.Audit
