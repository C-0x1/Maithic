(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema

type axiom_class = Standard | Sorry | Native | Other

let standard_axioms = [ "propext"; "Quot.sound"; "Classical.choice" ]
let legacy_native_axioms = [ "Lean.ofReduceBool"; "Lean.ofReduceNat"; "Lean.trustCompiler" ]

let classify name =
  if List.mem name standard_axioms then Standard
  else if name = "sorryAx" then Sorry
  else if List.mem name legacy_native_axioms
          || List.exists (fun part -> Text.starts_with ~prefix:"_native" part) (String.split_on_char '.' name)
  then Native
  else Other

type t = { name : string; allowed_axioms : string list; allow_native : bool; require_recheck : bool }

let default = { name = "default"; allowed_axioms = standard_axioms; allow_native = false; require_recheck = true }

let of_json doc =
  let table = match Json.member "trust" doc with Some t -> t | None -> doc in
  let known = [ "name"; "allowed_axioms"; "allow_native"; "require_recheck" ] in
  match table with
  | Json.Obj members ->
    (match List.find_opt (fun (k, _) -> not (List.mem k known)) members with
     | Some (k, _) -> Error (Printf.sprintf "unknown trust policy setting '%s'" k)
     | None ->
       let allowed =
         match Json.member "allowed_axioms" table with
         | None -> Ok default.allowed_axioms
         | Some (Json.List items) when List.for_all (function Json.String _ -> true | _ -> false) items ->
           Ok (Json.strings (Json.List items))
         | Some _ -> Error "trust policy setting 'allowed_axioms' must be a list of strings"
       in
       (match allowed with
        | Error _ as e -> e
        | Ok allowed_axioms ->
          if List.mem "sorryAx" allowed_axioms then Error "a trust policy cannot allow 'sorryAx'"
          else
            Ok
              { name = Json.string_or default.name "name" table;
                allowed_axioms;
                allow_native = Json.bool_or default.allow_native "allow_native" table;
                require_recheck = Json.bool_or default.require_recheck "require_recheck" table }))
  | _ -> Error "a trust policy must be a table"

let load path = Result.bind (Toml.parse_file path) of_json

let to_json p =
  Json.Obj
    [ ("name", Json.String p.name);
      ("allowed_axioms", Json.List (List.map (fun a -> Json.String a) p.allowed_axioms));
      ("allow_native", Json.Bool p.allow_native);
      ("require_recheck", Json.Bool p.require_recheck) ]

type facts = {
  declaration : string;
  found : bool;
  kind : string;
  axioms : string list;
  errors : int;
  rechecked : bool;
}

type decision = { status : Status.t; reasons : string list; violations : string list }

let decide policy facts =
  if not facts.found then
    { status = Status.Failed; reasons = [ Printf.sprintf "declaration '%s' was not produced" facts.declaration ]; violations = [] }
  else begin
    let violations =
      List.filter
        (fun a ->
          match classify a with
          | Sorry -> true
          | Native -> not policy.allow_native
          | Standard | Other -> not (List.mem a policy.allowed_axioms))
        facts.axioms
    in
    let reasons =
      (if facts.errors > 0 then [ Printf.sprintf "elaboration reported %d error(s)" facts.errors ] else [])
      @ (if facts.kind <> "theorem" then [ Printf.sprintf "'%s' is a %s, not a theorem" facts.declaration facts.kind ] else [])
      @ (if List.exists (fun a -> classify a = Sorry) facts.axioms then [ "the proof is unfinished: it depends on sorry" ] else [])
      @ (match List.filter (fun a -> classify a = Native) violations with
         | [] -> []
         | _ -> [ "the proof trusts compiled evaluation, which the policy does not allow" ])
      @ (match List.filter (fun a -> classify a = Other || classify a = Standard) violations with
         | [] -> []
         | outside -> [ "axioms outside the policy: " ^ String.concat ", " outside ])
    in
    if reasons <> [] then { status = Status.Failed; reasons; violations }
    else if policy.require_recheck && not facts.rechecked then
      { status = Status.Checked; reasons = [ "the fresh-process re-check required by the policy has not run" ]; violations = [] }
    else { status = Status.Verified; reasons = []; violations = [] }
  end

let decision_to_json d =
  Json.Obj
    [ ("status", Status.to_json d.status);
      ("reasons", Json.List (List.map (fun r -> Json.String r) d.reasons));
      ("violations", Json.List (List.map (fun r -> Json.String r) d.violations)) ]
