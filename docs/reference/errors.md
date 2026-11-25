# Error codes

Every error a command prints carries a code of the form `MTH-AREA-NNNN`, a message and, where there is one, a
hint that says what to do next. Without `--json` an error is a line on standard error. With `--json` the same
fields are in the `error` member of a document on standard output, with the exit status as `exit`; that holds
for a mistake in the arguments as well. [The scripting guide](../guides/scripting.md) has an example of each.

| Code | Meaning | Exit status |
|---|---|---:|
| `MTH-CLI-0001` | Wrong arguments. | 2 |
| `MTH-CLI-0002` | A file could not be read. | 2 |
| `MTH-CONFIG-0001` | Unknown setting; the message suggests the nearest known one. | 3 |
| `MTH-CONFIG-0002` | A value of the wrong type or outside its enumeration. | 3 |
| `MTH-CONFIG-0003` | A configuration file is not valid TOML; the message names the line. | 3 |
| `MTH-CONFIG-0004` | The file given with `--config` does not exist. | 3 |
| `MTH-BRIDGE-0001` | The Bridge executable was not found. | 4 |
| `MTH-BRIDGE-0002` | The Bridge process could not be started or ended unexpectedly. | 4 |
| `MTH-BRIDGE-0003` | The Bridge sent a response that does not follow the protocol. | 1 |
| `MTH-BRIDGE-0004` | The Bridge returned a typed error; its own code is in the message. | 1 |
| `MTH-BRIDGE-0005` | The Bridge did not answer within the deadline. | 10 |
| `MTH-BRIDGE-0006` | The Bridge speaks another protocol version, or the project pins another Lean version. | 4 |
| `MTH-PROVE-0001` | The theorem was not found in the file. | 2 |
| `MTH-PROVE-0002` | The file has errors before the theorem. | 1 |
| `MTH-LEDGER-0001` | The trust policy file could not be loaded. | 3 |
| `MTH-ORACLE-0001` | Julia or the package `MaithicOracle` was not found. | 4 |
| `MTH-ORACLE-0002` | The goal could not be read, or the goal of the theorem cannot be sent to an oracle. | 2 |
| `MTH-ORACLE-0003` | There is no oracle of the given name. | 2 |
| `MTH-ORACLE-0004` | The oracle did not answer: no oracle takes the goal, the Julia package could not be set up, the engine failed, or it ran out of time. | 1 |
| `MTH-BENCH-0001` | The benchmark manifest could not be read. | 2 |
| `MTH-BENCH-0002` | The file given to `bench gate` is not a benchmark run. | 2 |
| `MTH-BENCH-0003` | There is no baseline for a suite of the run; `--update` stores one. | 2 |
| `MTH-REFUTE-0001` | The theorem was not found in the file, or the file has no theorem with an unfinished proof and none was named. | 2 |
| `MTH-REFUTE-0002` | The statement tests are not on Lean's search path: the Bridge was copied away from the build of the Maithic Lean package, and the project does not depend on it. | 12 |
| `MTH-REFUTE-0003` | The file has errors before the theorem, or the statement does not elaborate; the message is Lean's. | 1 |
| `MTH-INTEL-0001` | There is no graph in the directory; `maithic intel scan PATH` writes one. | 2 |
| `MTH-INTEL-0002` | The file in the directory is not a code graph. | 2 |
| `MTH-INTEL-0003` | No declaration of that name in the graph, or several whose names end that way; the message lists them. | 2 |
| `MTH-CORPUS-0001` | The dataset could not be written. | 1 |
| `MTH-REVIEW-0001` | The review policy could not be read (exit status 3), or there is no profile of the given name (2). | 3, 2 |
| `MTH-GUIDE-0001` | Julia or the guidance package was not found. | 4 |
| `MTH-GUIDE-0002` | The model file could not be read, or is not a model. | 2 |
| `MTH-GUIDE-0003` | The guidance service did not start or did not answer, or the goal to rank could not be had. | 1 |
| `MTH-GUIDE-0004` | Training failed; the message is that of the trainer. | 1 |
| `MTH-MEND-0001` | The table of renames could not be read. | 2 |
| `MTH-MEND-0002` | Lean could not be asked about the file. | 1 |
| `MTH-MEND-0003` | The theorem to shorten was not found, or the file has no proof to shorten. | 2 |
| `MTH-FORMALIZE-0001` | The problem could not be read: no such file, text that is not UTF-8, an empty problem, or a problem given twice. | 2 |
| `MTH-FORMALIZE-0002` | Lean could not load the modules named with `--import`. | 1 |
| `MTH-FORMALIZE-0003` | The statement checks are not on Lean's search path; they are part of the Maithic Lean package. | 12 |
| `MTH-FORMALIZE-0004` | No statement could be drafted because the model could not be asked; the message says why. | 5 |
| `MTH-COUNCIL-0001` | The file has no theorem with an unfinished proof, or several; name one with `--theorem`. | 2 |
| `MTH-COUNCIL-0002` | The file has errors before the theorem; the message is Lean's. | 1 |
| `MTH-BLUEPRINT-0001` | The file is not a blueprint; the message says which line could not be read. | 2 |
| `MTH-BLUEPRINT-0002` | The blueprint has problems, such as a reference to a node that is not there or a circle; `maithic blueprint check` lists them. | 2 |
| `MTH-BLUEPRINT-0003` | No plan could be made: the model could not be asked, or its answer is not a plan. | 5 |
| `MTH-BLUEPRINT-0004` | Lean could not be asked about the skeleton or the Lean file. | 1 |
| `MTH-SEARCH-0001` | The declarations could not be read: the folder has no `declarations.jsonl`, or a Lean file could not be extracted. | 2 |
| `MTH-SEARCH-0002` | Lean's side of the search is not on Lean's search path; it is part of the Maithic Lean package. | 12 |
| `MTH-NNV-0001` | Julia or the package MaithicNNV was not found. | 4 |
| `MTH-NNV-0002` | A file is not a network or not a property: it is not JSON, or its schema is another. | 2 |
| `MTH-NNV-0003` | The job could not be run; the message is the verifier's or Lean's. | 1 |
| `MTH-ALGOLAB-0001` | The file is not a task: it is not TOML, or the name, the description or an example cannot be used. | 2 |
| `MTH-ALGOLAB-0002` | No implementation was drafted because the model could not be asked; the message says why. | 5 |
| `MTH-ALGOLAB-0003` | The program to test is not there; `lake build maithic-algo` builds it. | 4 |
| `MTH-ALGOLAB-0004` | Julia or the package MaithicAlgoRef was not found. | 4 |
| `MTH-ALGOLAB-0005` | The differential test could not be run; the message is the test's. | 1 |
| `MTH-GATEWAY-0001` | AI is disabled (gateway mode `off`). | 12 |
| `MTH-GATEWAY-0002` | Unknown provider. | 3 |
| `MTH-GATEWAY-0003` | Unknown model or tier. | 3 |
| `MTH-GATEWAY-0004` | No API key for the provider. | 5 |
| `MTH-GATEWAY-0005` | The provider rejected the key. | 5 |
| `MTH-GATEWAY-0006` | Rate limited. | 5 |
| `MTH-GATEWAY-0007` | No cassette for a request in replay mode. | 5 |
| `MTH-GATEWAY-0008` | The provider is overloaded. | 5 |
| `MTH-GATEWAY-0009` | The provider rejected the request. | 5 |
| `MTH-GATEWAY-0010` | Not found at the provider, for example an unknown model. | 5 |
| `MTH-GATEWAY-0011` | The provider call timed out. | 10 |
| `MTH-GATEWAY-0012` | The model declined to answer. | 5 |
| `MTH-GATEWAY-0013` | Provider error. | 5 |
| `MTH-GATEWAY-0014` | The provider could not be reached. | 5 |
| `MTH-GATEWAY-0015` | The provider's response could not be understood. | 5 |
| `MTH-GATEWAY-0020` | The model registry could not be loaded. | 3 |
| `MTH-GATEWAY-0021` | The credentials file could not be written. | 1 |

The codes of the Bridge server itself, `MTH-BRG-NNNN`, are listed in [`bridge-protocol.md`](bridge-protocol.md).
