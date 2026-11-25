-- runLengths, written by `maithic algo synth`.
-- Lean compiled the definition and ran it on the examples below; nothing is proved about it.

/-- The maximal runs of equal neighbours in `l`, in order, each as its value and its length; the empty list has no runs. -/
def runLengths (l : List Nat) : List (Nat × Nat) :=
  match l with
  | [] => []
  | x :: rest =>
    match runLengths rest with
    | (y, n) :: tail => if x = y then (y, n + 1) :: tail else (x, 1) :: (y, n) :: tail
    | [] => [(x, 1)]

#guard (runLengths [1, 1, 2, 3, 3, 3]) == ([(1, 2), (2, 1), (3, 3)])
#guard (runLengths []) == ([])
#guard (runLengths [7]) == ([(7, 1)])
#guard (runLengths [4, 4, 4, 4]) == ([(4, 4)])
#guard ((runLengths [5, 5, 6, 5]).length) == (3)
