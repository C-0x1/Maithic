(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What Lean says about one declaration.

    A repair of a proof and a shorter proof are both judged the same way:
    the text of the theorem, with the new proof in it, is elaborated where
    the theorem stood, and Lean's answer is read. That answer is a
    {!verdict}. The function that produces it is a parameter everywhere, so
    that the logic around it can be tested without Lean; in the command it
    is the Bridge. *)

open Maithic_bridge
open Maithic_ledger

type verdict = {
  accepted : bool;  (** Lean elaborated the declaration without an error. *)
  rejection : string;  (** Why the guard kept the text from Lean, when it did. *)
  error : string;  (** What Lean reported, when it did not accept the text. *)
  statement : string;  (** The statement of the declaration as Lean prints it; empty when Lean produced none. *)
  axioms : string list;  (** The axioms the declaration rests on. *)
  uses_sorry : bool;
}

type t = untrusted:string -> string -> verdict
(** Elaborates the text of one declaration in the environment before it.
    [untrusted] is the part of the text that did not come from the file: a
    proof a model wrote, a name from a table. It is screened by the guard
    before Lean sees the declaration. Text that is the file's own, with
    nothing added, is given with an empty [untrusted]. *)

let failed error = { accepted = false; rejection = ""; error; statement = ""; axioms = []; uses_sorry = false }

(** The verdict in a report of the Bridge, for the declaration called [name]. *)
let of_report ~name (report : Bridge_types.report) =
  let found = Recheck.find_decl report name in
  let statement = match found with Some d -> d.Bridge_types.type_ | None -> "" in
  let axioms = match found with Some d -> List.sort_uniq compare d.Bridge_types.axioms | None -> [] in
  let uses_sorry = match found with Some d -> d.Bridge_types.uses_sorry | None -> false in
  if not report.Bridge_types.ok then { (failed (Bridge_types.error_text report)) with statement; axioms; uses_sorry }
  else if found = None then failed "Lean accepted the text but did not produce the declaration"
  else { accepted = true; rejection = ""; error = ""; statement; axioms; uses_sorry }

(** The verdict in a report of the Bridge on a candidate. *)
let of_candidate ~name (candidate : Bridge_types.candidate_report) =
  if not candidate.Bridge_types.admitted then { (failed "") with rejection = candidate.Bridge_types.rejection }
  else match candidate.Bridge_types.report with None -> failed "the Bridge returned no report" | Some report -> of_report ~name report

(** A check that asks the Bridge, on an environment that holds the file up
    to the declaration. Text with an untrusted part goes through the guard
    as a candidate; the file's own text is elaborated as it is. *)
let on_bridge ?limits bridge ~env ~name : t =
 fun ~untrusted source ->
  if untrusted = "" then
    match Bridge_client.run_command ~names:[ name ] ?limits bridge ~env source with
    | Error e -> failed (Bridge_error.to_string e)
    | Ok (report, _) -> of_report ~name report
  else
    match Bridge_client.try_candidates ~names:[ name ] ?limits bridge ~env [ { Bridge_types.source; untrusted } ] with
    | Error e -> failed (Bridge_error.to_string e)
    | Ok (candidate :: _) -> of_candidate ~name candidate
    | Ok [] -> failed "the Bridge returned no report"

(** Whether a verdict is a proof of the same statement: accepted, without
    [sorry], and stating what the reference states. *)
let proves ~(reference : verdict) (v : verdict) = v.accepted && (not v.uses_sorry) && v.statement <> "" && v.statement = reference.statement

(** Why a verdict is not a proof of the reference statement, in a sentence. *)
let why_not ~(reference : verdict) (v : verdict) =
  if v.rejection <> "" then "refused by the guard: " ^ v.rejection
  else if not v.accepted then (match String.trim v.error with "" -> "Lean did not accept it" | e -> e)
  else if v.uses_sorry then "the proof rests on sorry"
  else if v.statement <> reference.statement then Printf.sprintf "the edit changes the statement, from %s to %s" reference.statement v.statement
  else ""
