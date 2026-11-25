(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Priors of a model in the search.

    The search asks a proposer for the tactics to try on a goal, each with a
    prior, and expands the open goal whose way from the root is the most
    probable. This module makes a proposer that keeps the candidates of
    another and lets a model weigh them: on goals that look like this one,
    which of these moves was made in the proofs it has seen?

    The model changes the order of a search and nothing else. The candidates
    are the same, each is still run by Lean, and a proof that is found is
    checked as a whole like any other. A guide that is wrong makes a search
    slower; it cannot make a proof wrong. *)

open Maithic_bridge
open Maithic_prover

val reweigh : (string * float) list -> float list -> (string * float) list
(** [reweigh base priors] gives each candidate of [base] the prior
    [b * p / m], where [b] is its prior in [base], [p] the probability the
    model gave it and [m] the largest of those probabilities. The model's
    favourite keeps its prior, and the others lose in proportion to how much
    less likely the model finds them; none falls to zero. The candidates
    stay in the order of [base]: the search tries all of them on a goal, and
    the priors decide which of the goals they leave is searched first.
    Equal probabilities change nothing, which is what makes a guide that is
    down the same as no guide. Neither does a list that does not fit the
    candidates. *)

val state_text : Bridge_types.goal -> string
(** A goal as the model reads it: as Lean prints it, hypotheses first and
    the target after the turnstile. *)

val proposer : Guide_advice.t -> base:Lean_search.proposer -> Lean_search.proposer
(** The candidates of [base], weighed by the guide. *)
