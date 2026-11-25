# Exit codes

Every `maithic` command ends with one of the codes below. The numbers are stable: scripts and
continuous-integration jobs may rely on them. `maithic help exit-codes` prints the same table, and
[the scripting guide](../guides/scripting.md) shows a script that decides by them.

| Code | Meaning |
|---:|---|
| 0 | Success |
| 1 | General failure |
| 2 | Usage error |
| 3 | Configuration error |
| 4 | Toolchain or runtime missing |
| 5 | Provider or authentication error |
| 6 | Budget exceeded |
| 7 | Abstained or not verified |
| 8 | Statement refuted |
| 9 | Trust or review policy violation |
| 10 | Timeout |
| 11 | Cancelled |
| 12 | Feature unavailable in this installation |
| 13 | Regression gate failed |

A command whose outcome is a status, such as `maithic prove`, exits with 0 only when every result
is `verified`. Any other status exits with 7, except `refuted`, which exits with 8.

The three codes that report an outcome, command by command:

| Command | 0 | 7 | 8 |
|---|---|---|---|
| `prove` | every theorem is verified | a theorem is not | a statement was refuted |
| `check` | no error, and every theorem is verified | a theorem is not | |
| `refute` | nothing was found, which is not a proof | a statement is vacuous, or a counterexample was not confirmed | a statement is refuted |
| `formalize` | a statement is chosen and no check flagged it | the chosen statement is flagged, or no candidate is left | |
| `council` | the claim is verified | the session ended without settling it | the claim is refuted |
| `oracle solve` | the goal is proved | the oracle does not know | the goal is refuted |
| `mend scan` | no file has an error | a file has an error | |
| `mend repair` | Lean accepts the whole file | something is left | |
| `review` | no finding is an error | a finding is an error | |
| `corpus verify` | the dataset is sound | it is not | |
| `blueprint check`, `skeleton` | the blueprint is sound; Lean compiles the skeleton | it has a problem; Lean rejects the skeleton | |
| `blueprint status --strict` | every node is done | a node is not | |
| `search` | something was found | nothing was | |
| `nnv verify` | the property is verified | the verifier cannot tell, or Lean did not accept its claim | the property is refuted |
| `algo synth` | a function was accepted | none was | |
| `algo differential` | every answer is accepted | an answer differs | |

Every error also carries a stable code of the form `MTH-AREA-NNNN`, printed with the message and,
with `--json`, returned in the `error.code` member.
