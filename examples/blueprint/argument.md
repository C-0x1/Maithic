# Odd numbers, squares and triangles

Write T(n) for the n-th triangular number, 1 + 2 + ... + n, and S(n) for the sum of the first n odd
numbers, 1 + 3 + ... + (2n - 1). Both are defined by recursion: T(n + 1) = T(n) + (n + 1) and
S(n + 1) = S(n) + (2n + 1).

Gauss's formula says that 2 T(n) = n (n + 1). It is proved by induction: in the step,
2 (T(n) + (n + 1)) = n (n + 1) + 2 (n + 1) = (n + 1) (n + 2).

The first n odd numbers add up to a square: S(n) = n^2. Again by induction: n^2 + (2n + 1) = (n + 1)^2.

Two triangular numbers that follow each other make a square: T(n) + T(n + 1) = (n + 1)^2. Indeed the
sum is 2 T(n) + (n + 1), which by Gauss's formula is n (n + 1) + (n + 1).

It follows that S(n + 1) = T(n) + T(n + 1): both sides are (n + 1)^2.
