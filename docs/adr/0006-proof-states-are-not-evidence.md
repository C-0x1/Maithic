# 6. A closed proof state is not a verified theorem

Status: accepted.

## Context

The Bridge can hold a proof at any point and run one tactic on it, which is what a search needs: thousands of
small steps from shared intermediate states, without elaborating the whole declaration each time. A proof state
whose goals are all closed looks like a proof. It was produced, however, by resuming Lean's elaborator from saved
internal state, a path that the kernel does not guard and that ordinary Lean files never take.

## Decision

Proof states are an aid to search and are never evidence.

- `startProof`, `runTactic` and `tryTactics` report goals and failures. None of them reports a status, and no
  result of theirs reaches the trust policy.
- When a search closes a state, the tactic script it found is written out as a complete declaration and sent
  through the same path as any candidate proof: the screen, elaboration from the imports with the kernel, the
  axiom audit, the trust policy, and the re-check in a fresh process.
- Tactic text passes the guard before it is parsed, as candidate proofs do.

## Consequences

- A defect in the state machinery can cost a search time. It cannot make a false statement verified.
- The step-wise path and the whole-declaration path can disagree, for instance when a tactic depends on
  elaboration order. The whole-declaration path wins, and the disagreement is recorded as a failed candidate.
- Errors are not recovered from inside a step: a tactic that would log an error and continue fails instead, so
  that a state never silently contains an admitted goal.
