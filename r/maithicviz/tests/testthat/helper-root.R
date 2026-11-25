# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The repository root: the nearest directory above the tests with PROJECT.toml.
repo_root <- function() {
  dir <- normalizePath(getwd())
  for (step in 1:12) {
    if (file.exists(file.path(dir, "PROJECT.toml"))) return(dir)
    dir <- dirname(dir)
  }
  NULL
}
