# 1. The OCaml core uses only the compiler's own libraries

Status: accepted.

## Context

The orchestrator needs JSON, TOML, SHA-256, a test framework, child processes and HTTP. Each has good libraries.
Each library is also a dependency to resolve on every machine and in every continuous-integration run, and a
piece of code that sits between a user's API key and the network.

## Decision

`ocaml/lib/base` implements what the core needs on top of the standard library and `unix`: a strict JSON parser
and printers, a TOML reader, SHA-256, file and process helpers and a small test framework with seeded property
tests. Live HTTP calls go through the `curl` command, with headers passed on standard input.

## Consequences

- Building the command needs a compiler and dune, nothing else. The trusted base is small and can be read.
- We maintain these modules ourselves. They are covered by known-answer tests, for example the standard SHA-256
  vectors, and by property tests such as "printing then parsing is the identity".
- Live mode needs `curl` on `PATH`. Replay, mock and off modes do not.
- Streaming responses are not supported by this transport; they will need a different one.
