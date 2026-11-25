# 3. Demonstration cassettes are authored, and say so

Status: accepted.

## Context

The model-assisted path must be runnable and testable with no key and no network. The usual way is to record
exchanges with a real provider and replay them. Recordings need a key, go stale when a prompt changes, and belong
to whoever paid for the calls.

## Decision

The cassettes in `fixtures/cassettes/` are written by hand. `fixtures/cassettes/authored.toml` lists, for each
demonstration, the answers to give; `make cassettes` runs the demonstration with those answers in place of a
model and stores each exchange for every provider. Each cassette carries `"origin": "authored"` and a note.

## Consequences

- The replay path is exercised end to end by the integration tests for all three providers.
- When a prompt changes, `make cassettes` regenerates the files, and the tests fail until it has been run.
- Authored answers show how the pipeline behaves. They are never reported as the performance of a model: the
  committed benchmark result uses no model at all.
