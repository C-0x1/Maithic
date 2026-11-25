# Council session on `sumOdd_eq_square`

## Opening

**1. coordinator → researcher, prover, critic, verifier** · inform · the problem: sumOdd_eq_square

> theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n

Refers to: #1 sumOdd_eq_square (statement); #2 the statement holds (claim)

**2. coordinator → verifier** · request · examine

> Check that the statement is well formed, look for a counterexample, and let automation try.

**3. verifier → coordinator** · verdict · the statement is well formed

> ∀ (n : Nat), sumOdd n = n * n

Refers to: #2 the statement holds (claim)

**4. verifier → coordinator** · inform · automation found no proof

> its automation found no proof in 18 attempts

## Round 1

**5. coordinator → researcher** · request · research

> Find the definitions and lemmas of the library that bear on the statement.

**6. researcher → prover, coordinator** · inform · 4 of 5 names exist in the library; 1 idea

> sumOdd : Nat → Nat
> Nat.succ_mul (n m : Nat) : n.succ * m = n * m + m
> Nat.mul_succ (n m : Nat) : n * m.succ = n * m + n
> Nat.mul_comm (n m : Nat) : n * m = m * n
> Lean does not know: Nat.add_mul_self_eq

Refers to: #3 sumOdd (definition); #4 Nat.succ_mul (definition); #5 Nat.mul_succ (definition); #6 Nat.mul_comm (definition); #7 an idea for the proof (note)

**7. coordinator → prover** · request · prove

> Propose proofs of the statement.

**8. prover → verifier** · propose · 2 candidate proofs

> by
>   induction n <;> simp [sumOdd]
>
> by
>   induction n with
>   | zero => rfl
>   | succ k ih => simp [sumOdd, ih]; omega

Refers to: #8 candidate 1 of round 1 (proof); #9 candidate 2 of round 1 (proof)

**9. coordinator → verifier** · request · verify

> Give each proposed proof to Lean, and say what Lean decided.

**10. verifier → coordinator, prover, critic** · verdict · proof 8 is rejected

> unsolved goals
> case succ
> n✝ : Nat
> a✝ : sumOdd n✝ = n✝ * n✝
> ⊢ sumOdd n✝ + (2 * n✝ + 1) = (n✝ + 1) * (n✝ + 1)

Refers to: #8 candidate 1 of round 1 (proof)

**11. verifier → coordinator, prover, critic** · verdict · proof 9 is rejected

> omega could not prove the goal:
> a possible counterexample may satisfy the constraints
>   c ≥ 0
>   b ≥ 0
>   a ≥ 0
>   a - b + 2*c ≤ -2
> where
>  a := ↑k * ↑k
>  b := ↑(k + 1) * ↑(k + 1)
>  c := ↑k

Refers to: #9 candidate 2 of round 1 (proof)

**12. coordinator → critic** · request · critique

> Say why the proofs Lean rejected fail, and what to try instead.

**13. critic → prover** · critique · a diagnosis of 2 rejected proofs, with 1 suggestion

> Both proofs get through the base case and stop in the step. After the induction hypothesis is used, the goal still holds the product (k + 1) * (k + 1). simp leaves it as it is, and omega cannot read it: omega decides linear arithmetic, and a product of two variables is not linear.
> Try: Rewrite the product with Nat.succ_mul and Nat.mul_succ first, so that k * k is the only product left on both sides, and call omega after that.

Refers to: #10 why the proofs of round 1 failed (note); #8 candidate 1 of round 1 (proof); #9 candidate 2 of round 1 (proof)

## Round 2

**14. coordinator → prover** · request · prove

> Propose proofs of the statement.

**15. prover → verifier** · propose · 1 candidate proof

> by
>   induction n with
>   | zero => rfl
>   | succ k ih =>
>     simp only [sumOdd, ih, Nat.succ_mul, Nat.mul_succ]
>     omega

Refers to: #11 candidate 1 of round 2 (proof)

**16. coordinator → verifier** · request · verify

> Give each proposed proof to Lean, and say what Lean decided.

**17. verifier → coordinator, prover, critic** · verdict · proof 11 is accepted: the claim is verified

> Lean's kernel accepted the proof. It rests on the axioms Quot.sound, propext.

Refers to: #11 candidate 1 of round 2 (proof); #2 the statement holds (claim)

**18. coordinator → all** · inform · the answer: verified

> Lean's kernel accepted proof 11, posted by the prover in round 2. It rests on the axioms Quot.sound, propext.

Refers to: #2 the statement holds (claim); #11 candidate 1 of round 2 (proof); #3 sumOdd (definition); #4 Nat.succ_mul (definition); #5 Nat.mul_succ (definition)

## Answer

**Verified.** Lean's kernel accepted proof 11, posted by the prover in round 2. It rests on the axioms Quot.sound, propext.

```lean
/-- The first `n` odd numbers add up to `n` squared. -/
theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by
  induction n with
  | zero => rfl
  | succ k ih =>
    simp only [sumOdd, ih, Nat.succ_mul, Nat.mul_succ]
    omega
```

Said in the session and not part of the answer: 1 definition, 2 proofs, 2 notes.
