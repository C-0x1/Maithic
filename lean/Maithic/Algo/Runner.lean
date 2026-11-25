/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Sort.Insertion
import Maithic.Algo.Sort.Linear
import Maithic.Algo.Sort.Binary
import Maithic.Algo.Sort.MergeSort
import Maithic.Algo.Sort.MinMax
import Maithic.Algo.Sort.Dedup
import Maithic.Algo.Sort.Selection
import Maithic.Algo.Sort.Match
import Maithic.Algo.Spec.Perm
import Maithic.Algo.Graph.Fib
import Maithic.Algo.Graph.PrefixSum
import Maithic.Algo.Graph.Path
import Maithic.Algo.Arith.Gcd
import Maithic.Algo.Arith.Pow
import Maithic.Algo.Cost.Comparisons

/-!
# The verified algorithms, one line at a time

A proof says that a function meets its specification. It does not say that the compiled program
is the function, or that the specification is the one that was meant. So the algorithms of this
library are also run: `answer` reads one request, a line of text, runs the algorithm it names on
the numbers that follow, and writes the result as a line. The executable `maithic-algo` does this
for every line of its input, and the differential tests in Julia compare what it writes with
reference implementations that share no code with it.

A request is the name of an algorithm and its arguments, whole numbers separated by blanks; a
bar separates two groups of arguments:

```
merge_sort 3 1 2            →  1 2 3
linear_search 2 | 3 1 2     →  2
binary_search 9 | 1 2 3     →  none
range_sum 1 3 | 5 6 7 8     →  13
path 4 0 3 | 0 1 1 2 2 3    →  0 1 2 3
```

A request that begins with `cost` asks how many comparisons an algorithm makes on its arguments,
for the four algorithms whose comparisons are counted and bounded in `Maithic.Algo.Cost`:

```
cost insertion_sort 3 2 1   →  3
cost binary_search 9 | 1 2 3  →  2
```
-/

namespace Maithic.Algo.Runner

/-- The whole numbers of a text; `none` when a word is not one. -/
def ints (s : String) : Option (List Int) :=
  ((s.splitOn " ").filter (· ≠ "")).mapM String.toInt?

/-- The natural numbers of a text; `none` when a word is not one. -/
def nats (s : String) : Option (List Nat) :=
  ((s.splitOn " ").filter (· ≠ "")).mapM String.toNat?

def showInts (l : List Int) : String := " ".intercalate (l.map toString)
def showNats (l : List Nat) : String := " ".intercalate (l.map toString)
def showIndex : Option Nat → String
  | some i => toString i
  | none => "none"

/-- Pairs of consecutive numbers: `[a, b, c, d]` is `[(a, b), (c, d)]`. -/
def pairs : List Nat → List (Nat × Nat)
  | a :: b :: rest => (a, b) :: pairs rest
  | _ => []

/-- The algorithms that can be asked for. -/
def algorithms : List String :=
  ["insertion_sort", "merge_sort", "selection_sort", "quick_sort", "linear_search", "binary_search", "minimum", "maximum",
   "dedup_sorted", "is_sorted", "same_counts", "fib", "prefix_sums", "range_sum", "gcd", "pow", "pow_mod", "find_sub", "path"]

/-- Runs one algorithm on its arguments, which come in groups. `none` when the arguments do not fit. -/
def run (name : String) (groups : List String) : Option String :=
  match name, groups with
  | "insertion_sort", [xs] => (ints xs).map fun l => showInts (insertionSort l)
  | "merge_sort", [xs] => (ints xs).map fun l => showInts (mergeSort l)
  | "selection_sort", [xs] => (ints xs).map fun l => showInts (selectionSort l)
  | "quick_sort", [xs] => (ints xs).map fun l => showInts (quickSort l)
  | "linear_search", [x, xs] => do
    let [target] ← ints x | none
    let l ← ints xs
    pure (showIndex (linearSearch target l))
  | "binary_search", [x, xs] => do
    let [target] ← ints x | none
    let l ← ints xs
    pure (showIndex (binarySearch l.toArray target))
  | "minimum", [xs] => (ints xs).map fun l => if h : l = [] then "none" else toString (minimum l h)
  | "maximum", [xs] => (ints xs).map fun l => if h : l = [] then "none" else toString (maximum l h)
  | "dedup_sorted", [xs] => (ints xs).map fun l => showInts (dedupSorted l)
  | "is_sorted", [xs] => (ints xs).map fun l => toString (isSorted l)
  | "same_counts", [xs, ys] => do
    let a ← ints xs
    let b ← ints ys
    pure (toString (countsAgree a b))
  | "fib", [n] => do
    let [k] ← nats n | none
    pure (toString (fibFast k))
  | "prefix_sums", [xs] => (ints xs).map fun l => showInts (prefixSums l)
  | "range_sum", [bounds, xs] => do
    let [i, j] ← nats bounds | none
    let l ← ints xs
    pure (toString (rangeSum (prefixSums l) i j))
  | "gcd", [ab] => do
    let [a, b] ← nats ab | none
    pure (toString (euclid a b))
  | "pow", [bn] => do
    let [b, n] ← nats bn | none
    pure (toString (fastPow b n))
  | "pow_mod", [bnm] => do
    let [b, n, m] ← nats bnm | none
    pure (toString (powMod b n m))
  | "find_sub", [pattern, text] => do
    let p ← ints pattern
    let t ← ints text
    pure (showIndex (findSub p t))
  | "path", [query, edges] => do
    let [n, u, target] ← nats query | none
    let es ← nats edges
    pure (match findPath (Graph.ofEdges (pairs es)) target n [] u with
      | some p => showNats p
      | none => "none")
  | _, _ => none

/-- The algorithms whose comparisons can be counted. -/
def costed : List String := ["linear_search", "binary_search", "insertion_sort", "merge_sort"]

/-- Counts the comparisons one algorithm makes on its arguments. `none` when the arguments do not fit. -/
def cost (name : String) (groups : List String) : Option String :=
  match name, groups with
  | "insertion_sort", [xs] => (ints xs).map fun l => toString (Cost.insertionSortCost l)
  | "merge_sort", [xs] => (ints xs).map fun l => toString (Cost.mergeSortCost l)
  | "linear_search", [x, xs] => do
    let [target] ← ints x | none
    let l ← ints xs
    pure (toString (Cost.linearSearchCost target l))
  | "binary_search", [x, xs] => do
    let [target] ← ints x | none
    let l ← ints xs
    pure (toString (Cost.binarySearchCost l.toArray target))
  | _, _ => none

/-- The groups of arguments in what follows the name of an algorithm. -/
def groupsOf (arguments : String) : List String :=
  (arguments.splitOn "|").map fun g => g.trimAscii.toString

/-- Answers a request for a count: the name of an algorithm and its arguments. -/
def answerCost (request : String) : String :=
  match request.splitOn " " with
  | [] | [""] => "error: cost needs the name of an algorithm"
  | name :: _ =>
    if !costed.contains name then s!"error: the comparisons of '{name}' are not counted"
    else
      match cost name (groupsOf (request.drop name.length).toString) with
      | some result => result
      | none => s!"error: the arguments of {name} do not fit"

/-- Answers one request. A request that cannot be answered gets a line that begins with `error:`. -/
def answer (line : String) : String :=
  let line := line.trimAscii.toString
  match line.splitOn " " with
  | [] | [""] => "error: an empty request"
  | name :: _ =>
    if name = "cost" then answerCost (line.drop name.length).toString.trimAscii.toString
    else if !algorithms.contains name then s!"error: '{name}' is not an algorithm of this library"
    else
      match run name (groupsOf (line.drop name.length).toString) with
      | some result => result
      | none => s!"error: the arguments of {name} do not fit"

end Maithic.Algo.Runner
