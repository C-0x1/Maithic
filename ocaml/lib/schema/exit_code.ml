(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Exit codes shared by every command. The numbers are stable: scripts and
    continuous-integration jobs may rely on them. *)

type t =
  | Success
  | Failure
  | Usage
  | Config
  | Toolchain
  | Provider
  | Budget
  | Not_verified
  | Refuted
  | Policy
  | Timeout
  | Cancelled
  | Unavailable
  | Regression

let all =
  [ Success; Failure; Usage; Config; Toolchain; Provider; Budget; Not_verified; Refuted; Policy; Timeout; Cancelled; Unavailable; Regression ]

let to_int = function
  | Success -> 0
  | Failure -> 1
  | Usage -> 2
  | Config -> 3
  | Toolchain -> 4
  | Provider -> 5
  | Budget -> 6
  | Not_verified -> 7
  | Refuted -> 8
  | Policy -> 9
  | Timeout -> 10
  | Cancelled -> 11
  | Unavailable -> 12
  | Regression -> 13

let describe = function
  | Success -> "Success"
  | Failure -> "General failure"
  | Usage -> "Usage error"
  | Config -> "Configuration error"
  | Toolchain -> "Toolchain or runtime missing"
  | Provider -> "Provider or authentication error"
  | Budget -> "Budget exceeded"
  | Not_verified -> "Abstained or not verified"
  | Refuted -> "Statement refuted"
  | Policy -> "Trust or review policy violation"
  | Timeout -> "Timeout"
  | Cancelled -> "Cancelled"
  | Unavailable -> "Feature unavailable in this installation"
  | Regression -> "Regression gate failed"

(** The exit code of a command whose outcome is a status. *)
let of_status = function
  | Status.Verified -> Success
  | Status.Refuted -> Refuted
  | Status.Checked | Status.Conjectured | Status.Unverified | Status.Unknown | Status.Failed | Status.Abstained ->
    Not_verified
