# 4. The language-share floors move toward their final values

Status: accepted; all four floors have reached their final values.

## Context

The project is written in OCaml, Lean, Julia and R, and the rule is that these four make up at least 85 percent
of the code by bytes, with a floor for each: OCaml 30, Lean 15, Julia 12, R 8. The parts are not built at the
same pace. The orchestrator, the Bridge client, the gateway and the repository tools came first and are OCaml;
the oracle, statistics and figure packages came next and are Julia and R; the verified Lean libraries grow one
proof at a time.

## Decision

`maithic-dev langshare` enforces the thresholds in `PROJECT.toml`. A floor is either at its final value or at a
stated interim value just below the measured share, and `PROJECT.toml` says which and why.

- The combined threshold and the floor for OCaml have always been at their final values.
- The floors for Julia and R started below their final values and reached 12 and 8 when the oracle and
  statistics packages were added.
- Those same packages outgrew the Lean libraries, whose share fell below 15. Rather than hold back tested code
  in two languages, the floor for Lean stood at 12 until the logic checkers were added, which brought the share
  of Lean to 19 percent and the floor back to 15.

## Consequences

- The gate always passes on the main branch, and every change to a floor is a visible change to
  `PROJECT.toml`.
- The README states which floors are below their final values for as long as any is. None is now.
- The final values are not negotiable: a release that is called complete has all four floors at 30, 15, 12
  and 8.
