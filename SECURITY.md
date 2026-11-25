# Security policy

## Reporting a vulnerability

Please report security problems privately to the maintainers at the address in [`SUPPORT.md`](SUPPORT.md), not in
a public issue. Include what you observed, how to reproduce it and the version. You will receive an
acknowledgement, and a fix or a mitigation plan will be coordinated with you before anything is published.

## What we consider a vulnerability

- An API key appearing in a log, an error message, a cassette, a result file or the environment of a Lean worker.
- Untrusted text, such as a model answer, causing a program to run, a file to be read or written, or a network
  connection to be opened.
- Any way to obtain the status `verified` for a statement that Lean's kernel did not accept under the policy in
  force, or for a statement other than the one requested.

## Design notes for reviewers

- Keys are looked up at the moment of a call and passed to `curl` on standard input. See
  `ocaml/lib/gateway/keys` and `ocaml/lib/gateway/core/transport.ml`.
- Candidate proofs are screened before elaboration in `ocaml/lib/bridge/sandbox/screen.ml` and again in
  `lean/MaithicBridge/Server/Guard.lean`. The screen is lexical. It is one layer, not a sandbox: do not run
  Maithic on files you do not trust, since a Lean file can execute code while it is elaborated.
- Lean workers are started with a reduced environment that never contains a credential.
- The status `verified` is produced only by `Trust_policy.decide`.

## Supported versions

Security fixes are made on the latest release.
