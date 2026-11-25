# 5. The Julia and R packages build on the standard distribution

Status: accepted.

## Context

The Julia packages compute certificates and the R packages compute statistics and figures. Both ecosystems have
excellent libraries for this work. Each dependency, however, is a registry that must be reachable when the tests
run, a version that can change under a result, and a body of code between a number and its reader. The claims
this project makes rest on checks that anyone can repeat, on any machine, years later.

## Decision

- The Julia packages depend on the standard library and on each other, by path (`[sources]` in `Project.toml`).
  `MaithicCore` supplies what they share: a strict JSON reader and a deterministic writer, the exact text form
  of numbers, the expression language of the oracle exchange format, and a random generator whose streams are
  fixed by its definition rather than by a Julia version. The Makefile runs the package manager offline.
- Algorithms that a certificate depends on are written here, in exact arithmetic where the certificate is
  exact: the simplex method, Buchberger's algorithm, Pratt proofs, interval arithmetic with directed rounding.
  Each is small because its output is checked, not trusted.
- The R packages use base R, `jsonlite`, and for figures `ggplot2` and `svglite`. Orderings never depend on
  the locale, and resampling is seeded.

## Consequences

- The Julia tests need no network and no registry; the R tests need four packages from CRAN.
- A capability whose plan names a third-party package is built on the standard distribution instead, and its
  entry in the register says what was built.
- Performance-critical paths may later gain optional accelerated back ends. The reference implementation, the
  one whose output the fixtures record, stays the dependency-free one.
