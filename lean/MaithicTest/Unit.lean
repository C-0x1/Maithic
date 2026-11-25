/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Run-time unit tests

The shape of a test that runs inside the test executable: a name, and a check that returns the
reason of a failure or nothing.
-/

namespace MaithicTest

/-- A unit test: a name and a check that returns the reason of a failure. -/
structure UnitTest where
  /-- The name, starting with the capability the test belongs to. -/
  name : String
  /-- The check. `none` means the test passed. -/
  run : IO (Option String)

end MaithicTest
