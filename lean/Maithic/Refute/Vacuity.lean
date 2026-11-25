/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Vacuity.Analysis
import Maithic.Refute.Vacuity.Command

/-!
# Vacuity and consistency checks

A statement with contradictory hypotheses is easy to prove and worth nothing. These checks look
for statements of that kind, and for their milder relatives: an empty domain, a conclusion that
needs no hypothesis, a hypothesis that the proof never uses or that testing shows to be
unnecessary.

| File | What it has |
|---|---|
| `Analysis` | the analyses, their results as text and as JSON |
| `Command` | `#maithic_vacuous`, `#maithic_necessity` and `#maithic_unused` |
-/
