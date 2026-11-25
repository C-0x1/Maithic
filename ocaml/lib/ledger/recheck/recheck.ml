(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Independent re-checking.

    A proof found in a long-lived session is checked once more before it is
    called verified: the finished text is elaborated from its imports in a
    new Bridge process that shares nothing with the session that produced
    it. The re-check must reach the same statement, and its facts, not the
    session's, are what the trust policy judges. *)

open Maithic_base
open Maithic_bridge

type outcome = {
  passed : bool;  (** the declaration exists and no error was reported *)
  declaration : string;
  statement : string;  (** the statement as Lean prints it in the fresh process *)
  kind : string;
  axioms : string list;
  errors : int;
  first_error : string;
  elapsed_ms : int;
  lean : string;  (** the Lean version of the fresh process *)
}

let short_name name = match List.rev (String.split_on_char '.' name) with last :: _ -> last | [] -> name

(** The declaration of a report that matches a name as written in source:
    the same name, or the name inside a namespace. *)
let find_decl (report : Bridge_types.report) name =
  let matches (d : Bridge_types.decl) =
    d.Bridge_types.found
    && (d.Bridge_types.name = name
        || Text.ends_with ~suffix:("." ^ name) d.Bridge_types.name
        || short_name d.Bridge_types.name = short_name name && List.length report.Bridge_types.decls = 1)
  in
  List.find_opt matches report.Bridge_types.decls

(** Elaborates [source] in a new Bridge process started with [argv] and
    reports on the declaration called [name]. *)
let fresh ?cwd ?timeout ?(limits = Bridge_types.no_limits) ~argv ~source name =
  let started = Unix.gettimeofday () in
  Bridge_client.with_bridge ?cwd ?timeout argv (fun bridge ->
      Result.map
        (fun ((report : Bridge_types.report), _) ->
          let elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
          let lean = (Bridge_client.handshake bridge).Bridge_types.lean in
          match find_decl report name with
          | Some d ->
            { passed = report.Bridge_types.ok; declaration = d.Bridge_types.name; statement = d.Bridge_types.type_;
              kind = d.Bridge_types.kind; axioms = d.Bridge_types.axioms; errors = report.Bridge_types.errors;
              first_error = Bridge_types.error_text report; elapsed_ms; lean }
          | None ->
            { passed = false; declaration = name; statement = ""; kind = ""; axioms = []; errors = max 1 report.Bridge_types.errors;
              first_error =
                (match Bridge_types.error_text report with "" -> "the declaration was not produced" | text -> text);
              elapsed_ms; lean })
        (Bridge_client.check_file ~limits bridge source))

let to_json o =
  Json.Obj
    [ ("passed", Json.Bool o.passed); ("declaration", Json.String o.declaration); ("statement", Json.String o.statement);
      ("axioms", Json.List (List.map (fun a -> Json.String a) o.axioms));
      ("errors", Json.Int o.errors); ("elapsed_ms", Json.Int o.elapsed_ms); ("lean", Json.String o.lean) ]
