# 2. The Bridge exposes environments, and checks whole declarations

Status: accepted.

## Context

A prover wants to try many candidate proofs of one theorem. Re-elaborating the file for each candidate is slow;
keeping one mutable session is fast but makes results depend on what ran before.

## Decision

The Bridge keeps immutable environments in a table and returns handles. `checkFile` and `runCommand` can keep the
environment they produce; `tryCandidates` elaborates each candidate on top of one stored environment and never
changes it. Import lists are deduplicated, so the same header is loaded once per process.

Candidates are complete declarations. Stepping through a proof tactic by tactic, with proof states as values, is
a later addition to the same protocol.

## Consequences

- Trying a candidate costs the candidate, not the file.
- A result never depends on the order of earlier requests, and the fresh-process re-check exists to catch any way
  in which it might.
- The Bridge reports facts, including the axioms a declaration depends on. It does not decide what is verified.
