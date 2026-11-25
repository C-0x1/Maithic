(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

let schema = "maithic.prove.search/1"

type outcome = Subgoals of int list | Rejected of { error_class : string; message : string } | Blocked of int
type edge = { tactic : string; prior : float; outcome : outcome }

type node = {
  id : int;
  parent : int option;
  depth : int;
  goal : string;
  hypotheses : string list;
  status : Search_tree.status;
  alias_of : int option;
  expanded : bool;
  abandoned : string option;
  edges : edge list;
  proved_by : int option;
}

type step = { node : int; started_ms : int; elapsed_ms : int }

type t = {
  statement : string;
  stop : string;
  root : int;
  nodes : node list;
  steps : step list;
  script : string list option;
  expansions : int;
  lean_calls : int;
  tokens : int;
  elapsed_ms : int;
}

let ms seconds = if Float.is_finite seconds then max 0 (int_of_float (Float.round (seconds *. 1000.0))) else 0

let of_result ?(statement = "") ~describe (result : 'a Search_kernel.result) =
  let tree = result.Search_kernel.tree in
  let nodes =
    List.init (Search_tree.size tree) (fun id ->
        let n = Search_tree.node tree id in
        let goal, hypotheses = describe n in
        let edges =
          List.map
            (fun (e : Search_tree.edge) ->
              let outcome =
                match e.Search_tree.outcome with
                | Search_tree.Subgoals ids -> Subgoals ids
                | Search_tree.Rejected { error_class; message } -> Rejected { error_class; message }
                | Search_tree.Blocked { node } -> Blocked node
              in
              { tactic = e.Search_tree.tactic; prior = e.Search_tree.prior; outcome })
            n.Search_tree.edges
        in
        { id; parent = n.Search_tree.parent; depth = n.Search_tree.depth; goal; hypotheses; status = Search_tree.effective tree id;
          alias_of = n.Search_tree.alias_of; expanded = n.Search_tree.expanded; abandoned = n.Search_tree.abandoned; edges;
          proved_by = n.Search_tree.proved_by })
  in
  let steps =
    List.map
      (fun (s : Search_kernel.step) ->
        { node = s.Search_kernel.node; started_ms = ms s.Search_kernel.started; elapsed_ms = ms s.Search_kernel.seconds })
      result.Search_kernel.steps
  in
  { statement; stop = Search_kernel.stop_to_string result.Search_kernel.stop; root = result.Search_kernel.root; nodes; steps;
    script = result.Search_kernel.script; expansions = result.Search_kernel.expansions; lean_calls = result.Search_kernel.lean_calls;
    tokens = result.Search_kernel.tokens; elapsed_ms = ms result.Search_kernel.seconds }

(* ---- JSON ---- *)

let optional name = function None -> [] | Some value -> [ (name, value) ]

let edge_to_json e =
  Json.Obj
    ([ ("tactic", Json.String e.tactic); ("prior", Json.Float e.prior) ]
    @
    match e.outcome with
    | Subgoals ids -> [ ("outcome", Json.String "subgoals"); ("subgoals", Json.List (List.map (fun i -> Json.Int i) ids)) ]
    | Rejected { error_class; message } ->
      [ ("outcome", Json.String "rejected"); ("error_class", Json.String error_class); ("message", Json.String message) ]
    | Blocked by -> [ ("outcome", Json.String "blocked"); ("by", Json.Int by) ])

let node_to_json n =
  Json.Obj
    ([ ("id", Json.Int n.id) ]
    @ optional "parent" (Option.map (fun i -> Json.Int i) n.parent)
    @ [ ("depth", Json.Int n.depth); ("goal", Json.String n.goal);
        ("hypotheses", Json.List (List.map (fun h -> Json.String h) n.hypotheses));
        ("status", Json.String (Search_tree.status_to_string n.status)); ("expanded", Json.Bool n.expanded) ]
    @ optional "alias_of" (Option.map (fun i -> Json.Int i) n.alias_of)
    @ optional "abandoned" (Option.map (fun why -> Json.String why) n.abandoned)
    @ optional "proved_by" (Option.map (fun i -> Json.Int i) n.proved_by)
    @ [ ("edges", Json.List (List.map edge_to_json n.edges)) ])

let to_json t =
  Json.Obj
    ([ ("schema", Json.String schema); ("statement", Json.String t.statement); ("stop", Json.String t.stop);
       ("expansions", Json.Int t.expansions); ("lean_calls", Json.Int t.lean_calls); ("tokens", Json.Int t.tokens);
       ("elapsed_ms", Json.Int t.elapsed_ms) ]
    @ optional "script" (Option.map (fun lines -> Json.List (List.map (fun l -> Json.String l) lines)) t.script)
    @ [ ("root", Json.Int t.root); ("nodes", Json.List (List.map node_to_json t.nodes));
        ( "steps",
          Json.List
            (List.map
               (fun s -> Json.Obj [ ("node", Json.Int s.node); ("started_ms", Json.Int s.started_ms); ("elapsed_ms", Json.Int s.elapsed_ms) ])
               t.steps) ) ])

let ( let* ) = Result.bind

let rec collect f = function
  | [] -> Ok []
  | x :: rest ->
    let* y = f x in
    let* ys = collect f rest in
    Ok (y :: ys)

let int_member name j = Option.bind (Json.member name j) Json.int_opt

let status_of_string = function
  | "open" -> Ok Search_tree.Open
  | "proved" -> Ok Search_tree.Proved
  | "failed" -> Ok Search_tree.Failed
  | other -> Error ("unknown status: " ^ other)

let edge_of_json j =
  let* tactic = Json.get_string "tactic" j in
  let* prior = match Option.bind (Json.member "prior" j) Json.float_opt with Some p -> Ok p | None -> Error "an edge has no prior" in
  let* kind = Json.get_string "outcome" j in
  let* outcome =
    match kind with
    | "subgoals" ->
      let* ids = Json.get_list "subgoals" j in
      let* ids = collect (fun i -> match Json.int_opt i with Some i -> Ok i | None -> Error "a subgoal is not a number") ids in
      Ok (Subgoals ids)
    | "rejected" ->
      let* error_class = Json.get_string "error_class" j in
      let* message = Json.get_string "message" j in
      Ok (Rejected { error_class; message })
    | "blocked" ->
      let* by = Json.get_int "by" j in
      Ok (Blocked by)
    | other -> Error ("unknown outcome: " ^ other)
  in
  Ok { tactic; prior; outcome }

let node_of_json j =
  let* id = Json.get_int "id" j in
  let* depth = Json.get_int "depth" j in
  let* goal = Json.get_string "goal" j in
  let* status = Result.bind (Json.get_string "status" j) status_of_string in
  let* expanded = Json.get_bool "expanded" j in
  let* edges = Result.bind (Json.get_list "edges" j) (collect edge_of_json) in
  Ok
    { id; parent = int_member "parent" j; depth; goal; hypotheses = Json.strings (Option.value ~default:(Json.List []) (Json.member "hypotheses" j));
      status; alias_of = int_member "alias_of" j; expanded; abandoned = Option.bind (Json.member "abandoned" j) Json.string_opt; edges;
      proved_by = int_member "proved_by" j }

let of_json j =
  let* named = Json.get_string "schema" j in
  let* () = if named = schema then Ok () else Error (Printf.sprintf "the document is %s, not %s" named schema) in
  let* stop = Json.get_string "stop" j in
  let* root = Json.get_int "root" j in
  let* nodes = Result.bind (Json.get_list "nodes" j) (collect node_of_json) in
  let* steps =
    Result.bind (Json.get_list "steps" j)
      (collect (fun s ->
           let* node = Json.get_int "node" s in
           let* started_ms = Json.get_int "started_ms" s in
           let* elapsed_ms = Json.get_int "elapsed_ms" s in
           Ok { node; started_ms; elapsed_ms }))
  in
  let count = List.length nodes in
  let known what i = if i >= 0 && i < count then Ok () else Error (Printf.sprintf "%s names node %d, and there are %d nodes" what i count) in
  let* () = known "the root" root in
  let* () =
    List.fold_left
      (fun acc (position, n) ->
        let* () = acc in
        let* () = if n.id = position then Ok () else Error (Printf.sprintf "node %d stands at position %d" n.id position) in
        let* () = match n.parent with Some p -> known "a parent" p | None -> Ok () in
        let* () = match n.alias_of with Some a -> known "an alias" a | None -> Ok () in
        let* () =
          match n.proved_by with
          | Some e when e < 0 || e >= List.length n.edges -> Error (Printf.sprintf "node %d is proved by an edge it does not have" n.id)
          | _ -> Ok ()
        in
        List.fold_left
          (fun acc e ->
            let* () = acc in
            match e.outcome with
            | Subgoals ids -> List.fold_left (fun acc i -> Result.bind acc (fun () -> known "a subgoal" i)) (Ok ()) ids
            | Blocked by -> known "a blocked edge" by
            | Rejected _ -> Ok ())
          (Ok ()) n.edges)
      (Ok ())
      (List.mapi (fun position n -> (position, n)) nodes)
  in
  let* () = List.fold_left (fun acc s -> Result.bind acc (fun () -> known "a step" s.node)) (Ok ()) steps in
  let script = Option.map Json.strings (Json.member "script" j) in
  Ok
    { statement = Json.string_or "" "statement" j; stop; root; nodes; steps; script; expansions = Json.int_or 0 "expansions" j;
      lean_calls = Json.int_or 0 "lean_calls" j; tokens = Json.int_or 0 "tokens" j; elapsed_ms = Json.int_or 0 "elapsed_ms" j }

(* ---- reading a record ---- *)

let node t id = List.nth_opt t.nodes id

let first_goals t =
  match node t t.root with Some { edges = { outcome = Subgoals ids; _ } :: _; _ } -> ids | _ -> []

let plural n one many = if n = 1 then Printf.sprintf "1 %s" one else Printf.sprintf "%d %s" n many

let summary t =
  Printf.sprintf "%s: %s expanded, %s to Lean, %s, %d ms" t.stop
    (plural t.expansions "goal" "goals")
    (plural t.lean_calls "call" "calls")
    (plural (List.length t.nodes) "node" "nodes")
    t.elapsed_ms

let steps_text t =
  let buf = Buffer.create 1024 in
  let width =
    List.fold_left
      (fun acc s -> match node t s.node with Some n -> List.fold_left (fun acc e -> max acc (Text.utf8_length e.tactic)) acc n.edges | None -> acc)
      0 t.steps
  in
  List.iteri
    (fun i s ->
      match node t s.node with
      | None -> ()
      | Some n ->
        Buffer.add_string buf (Printf.sprintf "step %d  goal #%d  %s  (%d ms)\n" (i + 1) n.id (Text.truncate 72 n.goal) s.elapsed_ms);
        List.iteri
          (fun position e ->
            let what =
              match e.outcome with
              | Subgoals [] -> "closes the goal"
              | Subgoals ids -> Printf.sprintf "leaves %s: %s" (plural (List.length ids) "goal" "goals") (String.concat ", " (List.map (Printf.sprintf "#%d") ids))
              | Rejected { error_class; _ } -> Printf.sprintf "fails (%s)" error_class
              | Blocked by -> Printf.sprintf "leads back to #%d" by
            in
            let mark = if n.proved_by = Some position then "*" else " " in
            Buffer.add_string buf (Printf.sprintf "  %s %s  %s\n" mark (Text.pad_right width e.tactic) what))
          n.edges)
    t.steps;
  Buffer.contents buf
