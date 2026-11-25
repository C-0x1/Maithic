(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The portfolio proposer: proofs that need no model.

    It enumerates a fixed list of proof scripts built from the automation
    that ships with Lean. The order is cheapest first and matches the
    [maithic_auto] tactic, so the prover finds without a key exactly what
    the tactic finds inside the editor. Because it is deterministic and
    free, the portfolio always runs before any model is asked. *)

open Maithic_base

(** The closing steps, in the order they are tried. *)
let steps = [ "rfl"; "trivial"; "simp"; "omega"; "decide"; "simp_all"; "grind" ]

(** Scripts that first take the goal apart and then close every piece. *)
let structured =
  [ "intros; constructor <;> simp_all";
    "intro h; cases h <;> simp_all";
    "intros; constructor <;> intros <;> omega";
    "intros; exact?" ]

(* The statement is a bare constant, as for a benchmark problem stated as a
   named proposition: the proof must unfold it first. *)
let head_constant header =
  match Text.find_sub header ":" with
  | None -> None
  | Some _ ->
    let after_colon =
      (* The type starts after the last top-level colon of the header. *)
      let depth = ref 0 and last = ref None in
      String.iteri
        (fun i c ->
          match c with
          | '(' | '[' | '{' -> incr depth
          | ')' | ']' | '}' -> decr depth
          | ':' when !depth = 0 -> last := Some i
          | _ -> ())
        header;
      match !last with Some i -> String.trim (String.sub header (i + 1) (String.length header - i - 1)) | None -> ""
    in
    let is_name =
      after_colon <> ""
      && String.for_all (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '.' -> true | _ -> false) after_colon
      && (match after_colon.[0] with 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false)
    in
    if is_name && not (List.mem after_colon [ "True"; "False"; "Prop" ]) then Some after_colon else None

(** The candidate proofs for a declaration header, best first, without
    duplicates. *)
let candidates header =
  let plain = List.map (fun step -> "by " ^ step) steps in
  let after_intros = List.map (fun step -> "by intros; " ^ step) steps in
  let unfolded =
    match head_constant header with
    | Some name -> List.map (fun step -> Printf.sprintf "by unfold %s; intros; %s" name step) steps
    | None -> []
  in
  unfolded @ plain @ after_intros @ List.map (fun script -> "by " ^ script) structured

let name = "portfolio"
