/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Search.Find

/-!
# Tests of `#maithic_find`

The command is run on a handful of declarations made for the purpose, where every finding can be
written down, and on the core library, where the lemmas a proof most often wants have to come
first.
-/

namespace MaithicTest.Search

open Lean Meta Elab Command Maithic.Search

/-- Something to find. -/
def pebbleAlpha : Nat := 1

theorem pebbleAlpha_beta : pebbleAlpha = 1 := rfl

theorem pebble_gamma (n : Nat) : n + pebbleAlpha = n + 1 := rfl

private theorem pebbleHidden : True := trivial

/-- Fails the build with a message when a check does not hold. -/
def expect (what : String) (holds : Bool) : CommandElabM Unit :=
  unless holds do throwError "check failed: {what}"

/-! ## What is found, and in which order -/

-- Every declaration whose name holds the part, with its statement; of two that match as well, the
-- shorter name first.
/--
info: MaithicTest.Search.pebbleAlpha : Nat
MaithicTest.Search.pebbleAlpha_beta : pebbleAlpha = 1
-/
#guard_msgs in
#maithic_find pebbleAlpha

-- All parts have to occur, in any order, and case does not matter.
/--
info: MaithicTest.Search.pebble_gamma : ∀ (n : Nat), n + pebbleAlpha = n + 1
-/
#guard_msgs in
#maithic_find GAMMA pebble

-- A part may be given in quotes, which is the way to give one with a dot or an underscore in it.
/--
info: MaithicTest.Search.pebbleAlpha_beta : pebbleAlpha = 1
-/
#guard_msgs in
#maithic_find "alpha_beta" "search.pebble"

-- A name that matches a part as a whole word comes before one that only holds it.
/--
info: MaithicTest.Search.pebble_gamma : ∀ (n : Nat), n + pebbleAlpha = n + 1
MaithicTest.Search.pebbleAlpha : Nat
MaithicTest.Search.pebbleAlpha_beta : pebbleAlpha = 1
-/
#guard_msgs in
#maithic_find MaithicTest pebble

-- No more than the limit.
/--
info: MaithicTest.Search.pebble_gamma : ∀ (n : Nat), n + pebbleAlpha = n + 1
-/
#guard_msgs in
#maithic_find (limit := 1) MaithicTest pebble

/-! ## What is not found -/

-- A private declaration is nobody else's to use.
/--
info: no declaration has a name with all of: pebbleHidden
-/
#guard_msgs in
#maithic_find pebbleHidden

/--
info: no declaration has a name with all of: pebble, nowhere
-/
#guard_msgs in
#maithic_find pebble nowhere

-- What Lean generates for its own use is left out: the equations of a definition, the pieces of a
-- structure, the auxiliary definitions of a proof.
run_cmd do
  let hits ← liftTermElabM (find #["pebble"] 100)
  expect "four declarations were made here, and three are to be found" (hits.size == 3)
  expect "none of them is an internal detail" (hits.all fun h => !h.name.isInternalDetail)

/-! ## As data -/

run_cmd do
  let hits ← liftTermElabM (find #["pebble", "gamma"])
  expect "one hit" (hits.size == 1)
  let hit := hits[0]!
  expect "its kind" (hit.kind == "theorem")
  expect "both parts are whole words of its name" (hit.whole == 2)
  expect "as JSON" (hit.toJson.compress ==
    "{\"kind\":\"theorem\",\"name\":\"MaithicTest.Search.pebble_gamma\",\"type\":\"∀ (n : Nat), n + pebbleAlpha = n + 1\",\"whole\":2}")
  let definition ← liftTermElabM (find #["pebblealpha"] 1)
  expect "a definition is a definition" (definition[0]!.kind == "definition")
  let nothing ← liftTermElabM (find #["", " "])
  expect "no part, no hit" (nothing.isEmpty || nothing.all fun h => (toString h.name).toLower.contains ' ')

/-! ## On the core library -/

run_cmd do
  let hits ← liftTermElabM (find #["succ", "mul"] 4)
  expect "the lemmas about natural numbers come first" ((hits.toList.map (·.name)).take 2 == [``Nat.mul_succ, ``Nat.succ_mul])
  expect "with their statements" (hits[1]!.type == "∀ (n m : Nat), n.succ * m = n * m + m")
  let comm ← liftTermElabM (find #["nat", "add_comm"] 3)
  expect "a name is found by its namespace and its last part" (comm[0]!.name == ``Nat.add_comm)
  let irrefl ← liftTermElabM (find #["Nat", "lt", "irrefl"])
  expect "three parts narrow it down to one" (irrefl.map (·.name) == #[``Nat.lt_irrefl])
  let none ← liftTermElabM (find #["zzzqqq"])
  expect "a part no name holds" none.isEmpty

end MaithicTest.Search
