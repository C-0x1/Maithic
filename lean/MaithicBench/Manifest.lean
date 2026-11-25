/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBench.Basics

/-!
# Suite manifest

The manifest of every registered problem, as a JSON string computed at compile time. The
executable `maithic-bench` prints it; `bench/suites/manifest.json` is its committed copy and the
build checks that the two agree.
-/

namespace MaithicBench

maithic_manifest_def manifest

end MaithicBench
