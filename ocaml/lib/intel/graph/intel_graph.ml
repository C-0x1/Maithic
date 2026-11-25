(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

let schema = "maithic.intel.graph/1"

type node_kind = Declaration | Module | Namespace | File
type edge_kind = Uses | Imports | Contains | Instance_of | Extends

type node = {
  id : string;
  kind : node_kind;
  name : string;
  internal : bool;
  decl_kind : string;
  file : string;
  line : int;
  end_line : int;
  name_line : int;
  uses_sorry : bool;
  contains_sorry : bool;
  axioms : string list;
  documented : bool;
}

type edge = { source : string; target : string; kind : edge_kind; via : string }

type t = {
  node_list : node list;
  edge_list : edge list;
  by_id : (string, node) Hashtbl.t;
  outgoing : (string, edge) Hashtbl.t;
  incoming : (string, edge) Hashtbl.t;
}

let node_kind_to_string = function Declaration -> "declaration" | Module -> "module" | Namespace -> "namespace" | File -> "file"
let edge_kind_to_string = function Uses -> "uses" | Imports -> "imports" | Contains -> "contains" | Instance_of -> "instance-of" | Extends -> "extends"

let node_kind_of_string = function
  | "declaration" -> Ok Declaration
  | "module" -> Ok Module
  | "namespace" -> Ok Namespace
  | "file" -> Ok File
  | other -> Error ("unknown kind of node: " ^ other)

let edge_kind_of_string = function
  | "uses" -> Ok Uses
  | "imports" -> Ok Imports
  | "contains" -> Ok Contains
  | "instance-of" -> Ok Instance_of
  | "extends" -> Ok Extends
  | other -> Error ("unknown kind of edge: " ^ other)

let prefix = function Declaration -> "d:" | Module -> "m:" | Namespace -> "n:" | File -> "f:"
let id_of kind name = prefix kind ^ name
let plain kind name = { id = id_of kind name; kind; name; internal = true; decl_kind = ""; file = ""; line = 0; end_line = 0; name_line = 0; uses_sorry = false; contains_sorry = false; axioms = []; documented = false }

let make node_list edge_list =
  let node_list = List.sort (fun (a : node) (b : node) -> compare a.id b.id) node_list in
  let edge_list =
    List.sort_uniq (fun (a : edge) (b : edge) -> compare (a.source, edge_kind_to_string a.kind, a.target, a.via) (b.source, edge_kind_to_string b.kind, b.target, b.via)) edge_list
  in
  let by_id = Hashtbl.create 1024 and outgoing = Hashtbl.create 4096 and incoming = Hashtbl.create 4096 in
  List.iter (fun (n : node) -> Hashtbl.replace by_id n.id n) node_list;
  (* Added in reverse, so that [find_all] returns the edges in their sorted order. *)
  List.iter
    (fun (e : edge) ->
      Hashtbl.add outgoing e.source e;
      Hashtbl.add incoming e.target e)
    (List.rev edge_list);
  { node_list; edge_list; by_id; outgoing; incoming }

let nodes t = t.node_list
let edges t = t.edge_list
let node t id = Hashtbl.find_opt t.by_id id

(* The proper prefixes of a dotted name, shortest first. *)
let namespaces name =
  let parts = String.split_on_char '.' name in
  let rec go acc built = function
    | [] | [ _ ] -> List.rev acc
    | part :: rest ->
      let here = if built = "" then part else built ^ "." ^ part in
      go (here :: acc) here rest
  in
  go [] "" parts

let build (extractions : Extraction.t list) =
  let found = Hashtbl.create 1024 and order = ref [] in
  let add (n : node) =
    if not (Hashtbl.mem found n.id) then begin
      Hashtbl.replace found n.id n;
      order := n.id :: !order
    end
  in
  let edges = ref [] in
  let edge source kind ?(via = "") target = edges := { source; target; kind; via } :: !edges in
  let own_modules = List.map (fun (e : Extraction.t) -> e.Extraction.module_name) extractions in
  (* Declarations first: a later reference to one of them must find the real node. *)
  List.iter
    (fun (e : Extraction.t) ->
      List.iter
        (fun (d : Extraction.declaration) ->
          add
            { id = id_of Declaration d.Extraction.name; kind = Declaration; name = d.Extraction.name; internal = true;
              decl_kind = d.Extraction.kind; file = e.Extraction.file; line = d.Extraction.line; end_line = d.Extraction.end_line;
              name_line = d.Extraction.name_line;
              uses_sorry = d.Extraction.uses_sorry; contains_sorry = Extraction.contains_sorry d; axioms = d.Extraction.axioms;
              documented = d.Extraction.doc <> None })
        e.Extraction.declarations)
    extractions;
  let outside name = add { (plain Declaration name) with internal = false } in
  List.iter
    (fun (e : Extraction.t) ->
      let file = id_of File e.Extraction.file and this = id_of Module e.Extraction.module_name in
      if e.Extraction.file <> "" then add { (plain File e.Extraction.file) with file = e.Extraction.file };
      if e.Extraction.module_name <> "" then begin
        add { (plain Module e.Extraction.module_name) with file = e.Extraction.file };
        if e.Extraction.file <> "" then edge file Contains this;
        List.iter
          (fun imported ->
            add { (plain Module imported) with internal = List.mem imported own_modules };
            edge this Imports (id_of Module imported))
          e.Extraction.imports
      end;
      List.iter
        (fun (d : Extraction.declaration) ->
          let name = d.Extraction.name in
          let me = id_of Declaration name in
          if e.Extraction.module_name <> "" then edge this Contains me else if e.Extraction.file <> "" then edge file Contains me;
          (* The chain of namespaces above the declaration. *)
          let chain = namespaces name in
          List.iter (fun ns -> add (plain Namespace ns)) chain;
          let rec link = function
            | outer :: (inner :: _ as rest) ->
              edge (id_of Namespace outer) Contains (id_of Namespace inner);
              link rest
            | [ last ] -> edge (id_of Namespace last) Contains me
            | [] -> ()
          in
          link chain;
          let in_type = d.Extraction.type_deps and in_value = d.Extraction.value_deps in
          List.iter
            (fun dep ->
              if dep <> name then begin
                outside dep;
                let via = match List.mem dep in_type, List.mem dep in_value with true, true -> "both" | true, false -> "type" | _ -> "value" in
                edge me Uses ~via (id_of Declaration dep)
              end)
            (List.sort_uniq compare (in_type @ in_value));
          Option.iter
            (fun cls ->
              outside cls;
              edge me Instance_of (id_of Declaration cls))
            d.Extraction.instance_of;
          List.iter
            (fun parent ->
              outside parent;
              edge me Extends (id_of Declaration parent))
            d.Extraction.parents)
        e.Extraction.declarations)
    extractions;
  make (List.map (Hashtbl.find found) (List.rev !order)) !edges

(* ---- JSON ---- *)

let node_to_json (n : node) =
  Json.Obj
    ([ ("id", Json.String n.id); ("kind", Json.String (node_kind_to_string n.kind)); ("name", Json.String n.name);
       ("internal", Json.Bool n.internal) ]
    @
    if n.kind <> Declaration then if n.file = "" then [] else [ ("file", Json.String n.file) ]
    else if not n.internal then []
    else
      [ ("declKind", Json.String n.decl_kind); ("file", Json.String n.file); ("line", Json.Int n.line); ("endLine", Json.Int n.end_line);
        ("nameLine", Json.Int n.name_line);
        ("usesSorry", Json.Bool n.uses_sorry); ("containsSorry", Json.Bool n.contains_sorry);
        ("axioms", Json.List (List.map (fun a -> Json.String a) n.axioms)); ("documented", Json.Bool n.documented) ])

let edge_to_json (e : edge) =
  Json.Obj
    ([ ("source", Json.String e.source); ("target", Json.String e.target); ("kind", Json.String (edge_kind_to_string e.kind)) ]
    @ if e.via = "" then [] else [ ("via", Json.String e.via) ])

let to_json t =
  Json.Obj
    [ ("schema", Json.String schema); ("nodes", Json.List (List.map node_to_json t.node_list));
      ("edges", Json.List (List.map edge_to_json t.edge_list)) ]

let ( let* ) = Result.bind

let rec collect f = function
  | [] -> Ok []
  | x :: rest ->
    let* y = f x in
    let* ys = collect f rest in
    Ok (y :: ys)

let of_json j =
  let* named = Json.get_string "schema" j in
  let* () = if named = schema then Ok () else Error (Printf.sprintf "the document is %s, not %s" named schema) in
  let* node_list =
    collect
      (fun n ->
        let* id = Json.get_string "id" n in
        let* kind = Result.bind (Json.get_string "kind" n) node_kind_of_string in
        let* name = Json.get_string "name" n in
        Ok
          { id; kind; name; internal = Json.bool_or true "internal" n; decl_kind = Json.string_or "" "declKind" n;
            file = Json.string_or "" "file" n; line = Json.int_or 0 "line" n; end_line = Json.int_or 0 "endLine" n;
            name_line = (match Json.int_or 0 "nameLine" n with 0 -> Json.int_or 0 "line" n | l -> l);
            uses_sorry = Json.bool_or false "usesSorry" n; contains_sorry = Json.bool_or false "containsSorry" n;
            axioms = Json.strings (Option.value ~default:(Json.List []) (Json.member "axioms" n));
            documented = Json.bool_or false "documented" n })
      (Json.list_or_empty "nodes" j)
  in
  let known = Hashtbl.create 1024 in
  List.iter (fun (n : node) -> Hashtbl.replace known n.id ()) node_list;
  let* edge_list =
    collect
      (fun e ->
        let* source = Json.get_string "source" e in
        let* target = Json.get_string "target" e in
        let* kind = Result.bind (Json.get_string "kind" e) edge_kind_of_string in
        if not (Hashtbl.mem known source) then Error ("an edge starts at a node that is not there: " ^ source)
        else if not (Hashtbl.mem known target) then Error ("an edge leads to a node that is not there: " ^ target)
        else Ok { source; target; kind; via = Json.string_or "" "via" e })
      (Json.list_or_empty "edges" j)
  in
  Ok (make node_list edge_list)

(* ---- looking things up ---- *)

let find t name =
  match node t (id_of Declaration name) with
  | Some n -> Ok n
  | None ->
    let suffix = "." ^ name in
    (match List.filter (fun (n : node) -> n.kind = Declaration && n.internal && Text.ends_with ~suffix n.name) t.node_list with
     | [ only ] -> Ok only
     | [] -> Error (Printf.sprintf "no declaration called '%s' in the graph" name)
     | several ->
       Error
         (Printf.sprintf "several declarations end in '%s': %s; give the full name" name
            (String.concat ", " (List.map (fun (n : node) -> n.name) several))))

let in_file t file =
  List.sort (fun (a : node) (b : node) -> compare (a.line, a.name) (b.line, b.name)) (List.filter (fun (n : node) -> n.kind = Declaration && n.internal && n.file = file) t.node_list)

let at t ~file ~line = List.find_opt (fun (n : node) -> n.line <= line && line <= n.end_line) (in_file t file)
let follow table pick t kind id = List.filter_map (fun (e : edge) -> if e.kind = kind then node t (pick e) else None) (Hashtbl.find_all table id)
let targets t kind id = follow t.outgoing (fun (e : edge) -> e.target) t kind id
let dependencies t name = targets t Uses (id_of Declaration name)
let dependents t name = follow t.incoming (fun (e : edge) -> e.source) t Uses (id_of Declaration name)
let contents t id = targets t Contains id

(* ---- statistics ---- *)

type degrees = { least : int; greatest : int; mean : float; median : float }

type statistics = {
  nodes_by_kind : (string * int) list;
  edges_by_kind : (string * int) list;
  declarations_by_kind : (string * int) list;
  external_declarations : int;
  used_by : degrees;
  uses : degrees;
  most_used : (string * int) list;
  most_used_internal : (string * int) list;
}

let tally keys =
  let counts = Hashtbl.create 16 in
  List.iter (fun k -> Hashtbl.replace counts k (1 + Option.value ~default:0 (Hashtbl.find_opt counts k))) keys;
  List.sort compare (Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts [])

let degrees = function
  | [] -> { least = 0; greatest = 0; mean = 0.0; median = 0.0 }
  | values ->
    let sorted = Array.of_list (List.sort compare values) in
    let n = Array.length sorted in
    let median = if n mod 2 = 1 then float_of_int sorted.(n / 2) else float_of_int (sorted.((n / 2) - 1) + sorted.(n / 2)) /. 2.0 in
    { least = sorted.(0); greatest = sorted.(n - 1); mean = float_of_int (Array.fold_left ( + ) 0 sorted) /. float_of_int n; median }

let statistics t =
  let declarations = List.filter (fun (n : node) -> n.kind = Declaration) t.node_list in
  let internal = List.filter (fun (n : node) -> n.internal) declarations in
  let count table id kind = List.length (List.filter (fun (e : edge) -> e.kind = kind) (Hashtbl.find_all table id)) in
  let top nodes =
    let used = List.map (fun (n : node) -> (n.name, count t.incoming n.id Uses)) nodes in
    List.filteri (fun i _ -> i < 10) (List.sort (fun (a, x) (b, y) -> compare (y, a) (x, b)) (List.filter (fun (_, c) -> c > 0) used))
  in
  { nodes_by_kind = tally (List.map (fun (n : node) -> node_kind_to_string n.kind) t.node_list);
    edges_by_kind = tally (List.map (fun (e : edge) -> edge_kind_to_string e.kind) t.edge_list);
    declarations_by_kind = tally (List.map (fun (n : node) -> n.decl_kind) internal);
    external_declarations = List.length declarations - List.length internal;
    used_by = degrees (List.map (fun (n : node) -> count t.incoming n.id Uses) internal);
    uses = degrees (List.map (fun (n : node) -> count t.outgoing n.id Uses) internal);
    most_used = top declarations; most_used_internal = top internal }

let statistics_to_json s =
  let counts pairs = Json.Obj (List.map (fun (k, v) -> (k, Json.Int v)) pairs) in
  let degrees d = Json.Obj [ ("least", Json.Int d.least); ("greatest", Json.Int d.greatest); ("mean", Json.Float d.mean); ("median", Json.Float d.median) ] in
  Json.Obj
    [ ("nodes", counts s.nodes_by_kind); ("edges", counts s.edges_by_kind); ("declarations", counts s.declarations_by_kind);
      ("externalDeclarations", Json.Int s.external_declarations); ("usedBy", degrees s.used_by); ("uses", degrees s.uses);
      ("mostUsed", Json.List (List.map (fun (name, by) -> Json.Obj [ ("name", Json.String name); ("usedBy", Json.Int by) ]) s.most_used));
      ( "mostUsedInternal",
        Json.List (List.map (fun (name, by) -> Json.Obj [ ("name", Json.String name); ("usedBy", Json.Int by) ]) s.most_used_internal) ) ]
