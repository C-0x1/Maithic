/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Runner

/-!
The executable `maithic-algo`: the verified algorithms as a program. It reads requests from its
standard input, one to a line, and writes one line for each: the result, or a line that begins
with `error:`. With `--list` it prints the names of the algorithms and stops.

```
$ echo "merge_sort 3 1 2" | lake exe maithic-algo
1 2 3
```
-/

open Maithic.Algo.Runner

partial def serve (input output : IO.FS.Stream) : IO Unit := do
  let line ← input.getLine
  if line.isEmpty then return
  output.putStrLn (answer line)
  output.flush
  serve input output

def main (args : List String) : IO UInt32 := do
  if args.contains "--list" then
    for name in algorithms do
      IO.println name
    return 0
  serve (← IO.getStdin) (← IO.getStdout)
  return 0
