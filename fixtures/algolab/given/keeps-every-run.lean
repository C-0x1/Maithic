def runLengths (l : List Nat) : List (Nat × Nat) :=
  match l with
  | [] => []
  | x :: rest =>
    match runLengths rest with
    | (y, n) :: tail => if x = y then (y, n + 1) :: tail else (x, 1) :: (y, n) :: tail
    | [] => [(x, 1)]
