(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type kind = Linear | Polynomial | Real
type domain = Integers | Rationals | Reals
type hypothesis = { name : string; statement : string }
type t = { kind : kind; domain : domain; variables : string list; hypotheses : hypothesis list; goal : string }

let schema = "maithic.oracle.goal/1"
let kind_to_string = function Linear -> "linear" | Polynomial -> "polynomial" | Real -> "real"
let kind_of_string = function "linear" -> Some Linear | "polynomial" -> Some Polynomial | "real" -> Some Real | _ -> None
let domain_to_string = function Integers -> "int" | Rationals -> "rat" | Reals -> "real"
let domain_of_string = function "int" -> Some Integers | "rat" -> Some Rationals | "real" -> Some Reals | _ -> None

let to_json g =
  Json.Obj
    [ ("schema", Json.String schema); ("kind", Json.String (kind_to_string g.kind)); ("domain", Json.String (domain_to_string g.domain));
      ("variables", Json.List (List.map (fun v -> Json.String v) g.variables));
      ( "hypotheses",
        Json.List (List.map (fun h -> Json.Obj [ ("name", Json.String h.name); ("statement", Json.String h.statement) ]) g.hypotheses) );
      ("goal", Json.String g.goal) ]

let ( let* ) = Result.bind

let of_json j =
  let* () = if Json.string_or "" "schema" j = schema then Ok () else Error ("the document does not name the schema " ^ schema) in
  let* kind =
    match kind_of_string (Json.string_or "" "kind" j) with
    | Some k -> Ok k
    | None -> Error "the kind of a goal is linear, polynomial or real"
  in
  let* domain =
    match Json.member "domain" j with
    | None -> Ok (if kind = Real then Reals else Rationals)
    | Some (Json.String d) -> (match domain_of_string d with Some d -> Ok d | None -> Error "the domain of a goal is int, rat or real")
    | Some _ -> Error "the domain of a goal is text"
  in
  let* hypotheses =
    List.fold_left
      (fun acc (i, entry) ->
        let* done_ = acc in
        match entry with
        | Json.String statement -> Ok (done_ @ [ { name = Printf.sprintf "h%d" (i + 1); statement } ])
        | Json.Obj _ ->
          (match Json.member "statement" entry with
           | Some (Json.String statement) -> Ok (done_ @ [ { name = Json.string_or (Printf.sprintf "h%d" (i + 1)) "name" entry; statement } ])
           | _ -> Error (Printf.sprintf "hypothesis %d has no statement" (i + 1)))
        | _ -> Error (Printf.sprintf "hypothesis %d is neither text nor an object" (i + 1)))
      (Ok [])
      (List.mapi (fun i e -> (i, e)) (Json.list_or_empty "hypotheses" j))
  in
  let* goal = match Json.member "goal" j with Some (Json.String g) -> Ok g | _ -> Error "the goal must be a comparison or \"false\"" in
  let variables = List.filter_map (function Json.String v -> Some v | _ -> None) (Json.list_or_empty "variables" j) in
  Ok { kind; domain; variables; hypotheses; goal }
