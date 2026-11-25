---
blueprint: 1
title: Odd numbers, squares and triangles
namespace: Triangles
---

## Definition `tri`: Triangular numbers

- lean: `tri`

The n-th triangular number is the sum 1 + 2 + ... + n, defined by recursion on n.

```lean
def tri : Nat → Nat
  | 0 => 0
  | n + 1 => tri n + (n + 1)
```

## Definition `sum-odd`: The sum of the first odd numbers

- lean: `sumOdd`

The sum of the first n odd numbers, 1 + 3 + ... + (2n - 1), defined by recursion on n.

```lean
def sumOdd : Nat → Nat
  | 0 => 0
  | n + 1 => sumOdd n + (2 * n + 1)
```

## Lemma `tri-step`: The next triangular number

- lean: `tri_succ`
- uses: `tri`

The triangular number after the n-th is the n-th plus n + 1.

```lean
(n : Nat) : tri (n + 1) = tri n + (n + 1)
```

**Proof.** This is the definition.

## Lemma `tri-formula`: Twice a triangular number

- lean: `two_mul_tri`
- uses: `tri`
- proof uses: `tri-step`

Twice the n-th triangular number is n (n + 1).

```lean
(n : Nat) : 2 * tri n = n * (n + 1)
```

**Proof.** By induction on n. In the step, 2 (tri n + (n + 1)) is n (n + 1) + 2 (n + 1) by the induction hypothesis, which is (n + 1) (n + 2).

## Lemma `odd-step`: The next odd number

- lean: `sumOdd_succ`
- uses: `sum-odd`

The sum of the first n + 1 odd numbers is the sum of the first n plus 2n + 1.

```lean
(n : Nat) : sumOdd (n + 1) = sumOdd n + (2 * n + 1)
```

**Proof.** This is the definition.

## Theorem `odd-square`: Odd numbers add up to a square

- lean: `sumOdd_eq_square`
- uses: `sum-odd`
- proof uses: `odd-step`

The first n odd numbers add up to n squared.

```lean
(n : Nat) : sumOdd n = n * n
```

**Proof.** By induction on n. In the step, n squared plus 2n + 1 is the square of n + 1.

## Lemma `two-triangles`: Two triangles make a square

- lean: `tri_add_tri_succ`
- uses: `tri`
- proof uses: `tri-formula`, `tri-step`

The n-th triangular number and the next add up to the square of n + 1.

```lean
(n : Nat) : tri n + tri (n + 1) = (n + 1) * (n + 1)
```

**Proof.** The sum is twice the n-th triangular number plus n + 1, which is n (n + 1) + (n + 1).

## Corollary `odd-triangles`: Odd numbers add up to two triangles

- lean: `sumOdd_eq_two_triangles`
- uses: `sum-odd`, `tri`
- proof uses: `odd-square`, `two-triangles`

The first n + 1 odd numbers add up to the n-th triangular number plus the next.

```lean
(n : Nat) : sumOdd (n + 1) = tri n + tri (n + 1)
```

**Proof.** Both sides are the square of n + 1.
