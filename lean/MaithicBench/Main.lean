/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBench.Manifest

/-!
# `maithic-bench`

```
maithic-bench manifest   print the JSON manifest of every benchmark problem
maithic-bench count      print the number of problems
```
-/

def main (args : List String) : IO UInt32 := do
  match args with
  | ["manifest"] =>
    IO.println MaithicBench.manifest
    return 0
  | ["count"] =>
    match Lean.Json.parse MaithicBench.manifest with
    | .ok j =>
      IO.println ((j.getObjValAs? Nat "count").toOption.getD 0)
      return 0
    | .error e =>
      IO.eprintln s!"maithic-bench: manifest is not valid JSON: {e}"
      return 1
  | _ =>
    IO.eprintln "Usage: maithic-bench (manifest | count)"
    return 2
