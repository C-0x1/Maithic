(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the search kernel on problem spaces given as tables. No Lean is
   involved: a goal is a name, and a table says which tactics apply to it and
   what each leaves behind. *)

open Maithic_base
open Maithic_prover
open Maithic_schema
open Testkit

type move = Leaves of string list | Fails

(* A problem space: for every goal, the tactics in the order a proposer would give them. *)
type world = (string * (string * float * move) list) list

let moves (world : world) goal = Option.value ~default:[] (List.assoc_opt goal world)

(* Expands a node by the table, and writes down the order of expansions. *)
let expander ?(calls = 1) ?(tokens = 0) (world : world) order (node : string Search_tree.node) =
  order := !order @ [ node.Search_tree.payload ];
  Ok
    { Search_kernel.tried =
        List.map
          (fun (tactic, prior, move) ->
            { Search_kernel.tactic; prior;
              step =
                (match move with
                 | Leaves goals -> Search_tree.Leaves (List.map (fun g -> (g, g)) goals)
                 | Fails -> Search_tree.Fails { error_class = "tactic"; message = tactic ^ " does not apply" }) })
          (moves world node.Search_tree.payload);
      lean_calls = calls; tokens }

let run ?now ?strategy ?budget ?calls ?tokens world goals =
  let order = ref [] in
  let result =
    match
      Search_kernel.search ?now ?strategy ?budget ~expand:(expander ?calls ?tokens world order) ~root:"" (List.map (fun g -> (g, g)) goals)
    with
    | Ok r -> r
    | Error (_ : string) -> fail "the expander does not fail"
  in
  ignore (is_ok "the tree follows its rules" (Search_tree.check_invariants result.Search_kernel.tree));
  (result, !order)

(* The goals of a space that can be proved at all, as the least fixed point of the rule. *)
let provable (world : world) =
  let proved = Hashtbl.create 16 in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (goal, tactics) ->
        if (not (Hashtbl.mem proved goal))
           && List.exists (function _, _, Leaves gs -> List.for_all (Hashtbl.mem proved) gs | _ -> false) tactics
        then begin
          Hashtbl.replace proved goal ();
          changed := true
        end)
      world
  done;
  fun goal -> Hashtbl.mem proved goal

(* Replays a script against the table: every line must be a tactic of the goal in focus, and at
   the end nothing may be left. Bullets open one subgoal each. *)
let replays (world : world) goals script =
  let rec strip line =
    let line = String.trim line in
    if String.length line >= 3 && String.sub line 0 3 = "\xC2\xB7 " then strip (String.sub line 3 (String.length line - 3)) else line
  in
  let rec go stack lines =
    match stack, lines with
    | [], [] -> true
    | goal :: rest, line :: lines ->
      (match List.find_opt (fun (tactic, _, _) -> tactic = strip line) (moves world goal) with
       | Some (_, _, Leaves subgoals) -> go (subgoals @ rest) lines
       | _ -> false)
    | _ -> false
  in
  go goals script

let chain = [ ("g", [ ("step", 0.9, Leaves [ "h" ]) ]); ("h", [ ("fail", 0.5, Fails); ("close", 0.5, Leaves []) ]) ]

(* Two ways from g: the likely tactic leads to a longer proof than the unlikely one. *)
let fork =
  [ ("g", [ ("likely", 0.8, Leaves [ "a" ]); ("unlikely", 0.1, Leaves [ "b" ]) ]);
    ("a", [ ("on", 0.9, Leaves [ "a2" ]) ]);
    ("a2", [ ("done", 0.9, Leaves []) ]);
    ("b", [ ("done", 0.9, Leaves []) ]) ]

let split =
  [ ("g", [ ("constructor", 0.9, Leaves [ "left"; "right" ]) ]);
    ("left", [ ("exact hp", 0.9, Leaves []) ]);
    ("right", [ ("cases h", 0.9, Leaves [ "r1"; "r2" ]) ]);
    ("r1", [ ("simp", 0.9, Leaves [ "r1'" ]) ]);
    ("r1'", [ ("rfl", 0.9, Leaves []) ]);
    ("r2", [ ("omega", 0.9, Leaves []) ]) ]

let tree_tests =
  [ case "P01-01-01 the tree keeps every state with its parent and every tactic with its outcome" (fun () ->
        let result, _ = run chain [ "g" ] in
        let tree = result.Search_kernel.tree in
        equal_int "nodes: the root, g and h" 3 (Search_tree.size tree);
        let g = Search_tree.node tree 1 and h = Search_tree.node tree 2 in
        equal_string "key" "g" g.Search_tree.key;
        check "parent link" (h.Search_tree.parent = Some 1 && g.Search_tree.parent = Some 0);
        equal_int "depth" 2 h.Search_tree.depth;
        (match h.Search_tree.edges with
         | [ { Search_tree.tactic = "fail"; outcome = Search_tree.Rejected { error_class = "tactic"; message }; _ };
             { Search_tree.tactic = "close"; outcome = Search_tree.Subgoals []; _ } ] ->
           equal_string "the message of the failure is kept" "fail does not apply" message
         | _ -> fail "the edges of h are a failure and a closing tactic, in the order tried");
        check "cost is the sum of -log prior" (Float.abs (h.Search_tree.cost +. Float.log 0.9) < 1e-12));
    case "P01-01-01 priors outside (0, 1] are clamped" (fun () ->
        let tree = Search_tree.create () in
        let root = Search_tree.add_root tree ~key:"g" () in
        let zero = Search_tree.add_edge tree root ~tactic:"a" ~prior:0.0 (Search_tree.Leaves [ ("x", ()) ]) in
        let big = Search_tree.add_edge tree root ~tactic:"b" ~prior:7.0 (Search_tree.Leaves [ ("y", ()) ]) in
        check "zero becomes the smallest positive number" (zero.Search_tree.prior > 0.0);
        check "above one becomes one" (big.Search_tree.prior = 1.0);
        check "a certain tactic adds no cost" ((Search_tree.node tree 2).Search_tree.cost = 0.0));
    case "P01-01-10 the invariant checker notices a status that does not follow from the edges" (fun () ->
        let tree = Search_tree.create () in
        let root = Search_tree.add_root tree ~key:"g" () in
        ignore (Search_tree.add_edge tree root ~tactic:"close" ~prior:1.0 (Search_tree.Leaves []));
        (* The node has a closing tactic but was never told to look: it is still open. *)
        contains "an open node with a proved edge" (is_error "stale status" (Search_tree.check_invariants tree)) "open node has a proved edge";
        Search_tree.finish tree root;
        ignore (is_ok "after the update" (Search_tree.check_invariants tree));
        check "proved" ((Search_tree.node tree root).Search_tree.status = Search_tree.Proved)) ]

let strategy_tests =
  [ case "P01-01-02 best-first expands the most probable path first" (fun () ->
        let result, order = run ~strategy:Search_kernel.Best_first fork [ "g" ] in
        (* a (0.8) before b (0.1); a2 (0.72) before b as well, and the proof through a is found first. *)
        equal_strings "order" [ "g"; "a"; "a2" ] order;
        equal_strings "script" [ "likely"; "on"; "done" ] (Option.get result.Search_kernel.script));
    case "P01-01-02 a likely start does not hide a better path" (fun () ->
        let world =
          [ ("g", [ ("likely", 0.6, Leaves [ "a" ]); ("other", 0.4, Leaves [ "b" ]) ]);
            ("a", [ ("weak", 0.1, Leaves [ "a2" ]) ]);
            ("a2", [ ("done", 1.0, Leaves []) ]);
            ("b", [ ("done", 1.0, Leaves []) ]) ]
        in
        let result, order = run ~strategy:Search_kernel.Best_first world [ "g" ] in
        (* a costs 0.6, b 0.4, a2 0.06: after a comes b, whose proof ends the search. *)
        equal_strings "order" [ "g"; "a"; "b" ] order;
        equal_strings "script" [ "other"; "done" ] (Option.get result.Search_kernel.script));
    case "P01-01-02 equal costs are taken in the order the nodes were made" (fun () ->
        let world = [ ("g", [ ("x", 0.5, Leaves [ "a" ]); ("y", 0.5, Leaves [ "b" ]) ]); ("a", []); ("b", [ ("done", 1.0, Leaves []) ]) ] in
        let _, order = run ~strategy:Search_kernel.Best_first world [ "g" ] in
        equal_strings "order" [ "g"; "a"; "b" ] order);
    case "P01-01-03 breadth-first goes level by level and finds a shortest proof" (fun () ->
        let result, order = run ~strategy:(Search_kernel.Breadth_first { width = 100 }) fork [ "g" ] in
        equal_strings "order" [ "g"; "a"; "b" ] order;
        equal_strings "script" [ "unlikely"; "done" ] (Option.get result.Search_kernel.script));
    case "P01-01-03 the width cap gives up the nodes of a level beyond it" (fun () ->
        let result, order = run ~strategy:(Search_kernel.Breadth_first { width = 1 }) fork [ "g" ] in
        equal_strings "only the first node of each level" [ "g"; "a"; "a2" ] order;
        equal_strings "script" [ "likely"; "on"; "done" ] (Option.get result.Search_kernel.script);
        let b = Search_tree.node result.Search_kernel.tree 3 in
        check "b was given up" (b.Search_tree.abandoned = Some "width cap" && b.Search_tree.status = Search_tree.Failed));
    case "P01-01-04 depth-first follows the newest node and backs up when a branch is spent" (fun () ->
        let world =
          [ ("g", [ ("left", 0.5, Leaves [ "a" ]); ("right", 0.5, Leaves [ "b" ]) ]);
            ("a", [ ("down", 0.5, Leaves [ "a2" ]) ]);
            ("a2", [ ("stuck", 0.5, Fails) ]);
            ("b", [ ("done", 0.5, Leaves []) ]) ]
        in
        let result, order = run ~strategy:(Search_kernel.Depth_first { depth = 10; branching = 10 }) world [ "g" ] in
        equal_strings "the left branch to its end, then the right" [ "g"; "a"; "a2"; "b" ] order;
        equal_strings "script" [ "right"; "done" ] (Option.get result.Search_kernel.script));
    case "P01-01-04 the depth limit ends a branch that would never end" (fun () ->
        (* Every goal n leads to goal n + 1. *)
        let endless = List.init 50 (fun i -> (string_of_int i, [ ("next", 0.9, Leaves [ string_of_int (i + 1) ]) ])) in
        let result, order = run ~strategy:(Search_kernel.Depth_first { depth = 5; branching = 10 }) endless [ "0" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted);
        equal_strings "five levels" [ "0"; "1"; "2"; "3"; "4" ] order;
        check "the node past the limit was given up" ((Search_tree.node result.Search_kernel.tree 6).Search_tree.abandoned = Some "depth limit"));
    case "P01-01-04 the branching limit follows only the first tactics that work" (fun () ->
        let world =
          [ ("g", [ ("bad", 0.3, Fails); ("first", 0.3, Leaves [ "a" ]); ("second", 0.3, Leaves [ "b" ]) ]);
            ("a", [ ("stuck", 0.5, Fails) ]);
            ("b", [ ("done", 0.5, Leaves []) ]) ]
        in
        let narrow, order = run ~strategy:(Search_kernel.Depth_first { depth = 10; branching = 1 }) world [ "g" ] in
        equal_strings "b is never tried" [ "g"; "a" ] order;
        check "no proof with one branch" (narrow.Search_kernel.stop = Search_kernel.Exhausted);
        let wide, _ = run ~strategy:(Search_kernel.Depth_first { depth = 10; branching = 2 }) world [ "g" ] in
        equal_strings "with two" [ "second"; "done" ] (Option.get wide.Search_kernel.script)) ]

let budget_tests =
  let endless = List.init 200 (fun i -> (string_of_int i, [ ("next", 0.9, Leaves [ string_of_int (i + 1) ]) ])) in
  [ case "P01-01-05 the search stops when its expansions are spent" (fun () ->
        let result, order = run ~budget:{ Search_kernel.unlimited with Search_kernel.expansions = 7 } endless [ "0" ] in
        check "out of expansions" (result.Search_kernel.stop = Search_kernel.Out_of "expansions");
        equal_int "exactly seven" 7 (List.length order);
        equal_int "counted" 7 result.Search_kernel.expansions;
        check "no script" (result.Search_kernel.script = None));
    case "P01-01-05 time is checked before every expansion" (fun () ->
        (* A clock that advances one second each time it is read. It is read once at the start,
           and three times for every expansion: for the budget, before the expansion and after it. *)
        let ticks = ref 0.0 in
        let now () = ticks := !ticks +. 1.0; !ticks in
        let result, order = run ~now ~budget:{ Search_kernel.unlimited with Search_kernel.seconds = 5.0 } endless [ "0" ] in
        check "out of time" (result.Search_kernel.stop = Search_kernel.Out_of "time");
        equal_int "two expansions begin before the fifth second" 2 (List.length order);
        equal_int "and both are on record" 2 (List.length result.Search_kernel.steps));
    case "P01-01-05 calls to Lean and tokens are counted as the expansions report them" (fun () ->
        let by_calls, order = run ~calls:3 ~budget:{ Search_kernel.unlimited with Search_kernel.lean_calls = 10 } endless [ "0" ] in
        check "out of calls" (by_calls.Search_kernel.stop = Search_kernel.Out_of "lean calls");
        equal_int "four expansions of three calls reach ten" 4 (List.length order);
        equal_int "calls" 12 by_calls.Search_kernel.lean_calls;
        let by_tokens, order = run ~tokens:250 ~budget:{ Search_kernel.unlimited with Search_kernel.tokens = 1000 } endless [ "0" ] in
        check "out of tokens" (by_tokens.Search_kernel.stop = Search_kernel.Out_of "tokens");
        equal_int "four expansions of 250 tokens" 4 (List.length order);
        equal_int "tokens" 1000 by_tokens.Search_kernel.tokens);
    case "P01-01-05 a budget of nothing expands nothing" (fun () ->
        let result, order = run ~budget:{ Search_kernel.unlimited with Search_kernel.expansions = 0 } chain [ "g" ] in
        check "out of expansions" (result.Search_kernel.stop = Search_kernel.Out_of "expansions");
        equal_strings "nothing was expanded" [] order);
    case "P01-01-05 a proof found within the budget is returned with what it cost" (fun () ->
        let result, _ = run ~calls:2 ~tokens:5 ~budget:{ Search_kernel.unlimited with Search_kernel.expansions = 2 } chain [ "g" ] in
        check "found" (result.Search_kernel.stop = Search_kernel.Found);
        equal_int "expansions" 2 result.Search_kernel.expansions;
        equal_int "calls" 4 result.Search_kernel.lean_calls;
        equal_int "tokens" 10 result.Search_kernel.tokens;
        equal_string "names" "found, exhausted, out of time"
          (String.concat ", " (List.map Search_kernel.stop_to_string [ Search_kernel.Found; Search_kernel.Exhausted; Search_kernel.Out_of "time" ]))) ]

let and_or_tests =
  [ case "P01-01-06 a tactic that leaves two goals is proved when both are" (fun () ->
        let result, order = run split [ "g" ] in
        check "found" (result.Search_kernel.stop = Search_kernel.Found);
        equal_int "every goal was expanded once" 6 (List.length order);
        let half = List.filter (fun (goal, _) -> goal <> "r2") split in
        let stuck, _ = run half [ "g" ] in
        check "with one subgoal open the goal is not proved" (stuck.Search_kernel.stop = Search_kernel.Exhausted));
    case "P01-01-06 several goals at the start are proved together" (fun () ->
        let result, _ = run split [ "left"; "r2" ] in
        equal_strings "one bullet each" [ "\xC2\xB7 exact hp"; "\xC2\xB7 omega" ] (Option.get result.Search_kernel.script);
        let stuck, _ = run split [ "left"; "nowhere" ] in
        check "all of them or none" (stuck.Search_kernel.stop = Search_kernel.Exhausted));
    case "P01-01-07 the proof is the tactics of the winning edges, with bullets where goals split" (fun () ->
        let result, _ = run split [ "g" ] in
        equal_strings "script"
          [ "constructor"; "\xC2\xB7 exact hp"; "\xC2\xB7 cases h"; "  \xC2\xB7 simp"; "    rfl"; "  \xC2\xB7 omega" ]
          (Option.get result.Search_kernel.script);
        check "the script proves the goal in the table" (replays split [ "g" ] (Option.get result.Search_kernel.script)));
    case "P01-01-07 a proof below reaches the root as soon as the last goal closes" (fun () ->
        let result, _ = run split [ "g" ] in
        let tree = result.Search_kernel.tree in
        check "every node on the way is proved"
          (List.for_all (fun id -> (Search_tree.node tree id).Search_tree.status = Search_tree.Proved) (List.init (Search_tree.size tree) Fun.id));
        check "the root names its edge" ((Search_tree.node tree 0).Search_tree.proved_by = Some 0));
    case "P01-01-07 the first tactic that works is the one in the script, not a later one" (fun () ->
        let world = [ ("g", [ ("a", 0.5, Leaves []); ("b", 0.5, Leaves []) ]) ] in
        let result, _ = run world [ "g" ] in
        equal_strings "script" [ "a" ] (Option.get result.Search_kernel.script));
    case "P01-01-08 a goal with no working tactic fails, and so does the tactic that led to it" (fun () ->
        let world =
          [ ("g", [ ("split", 0.9, Leaves [ "dead"; "costly" ]); ("other", 0.1, Leaves [ "fine" ]) ]);
            ("dead", [ ("x", 0.5, Fails); ("y", 0.5, Fails) ]);
            ("costly", [ ("on", 0.9, Leaves [ "costly2" ]) ]);
            ("costly2", [ ("on", 0.9, Leaves [ "costly3" ]) ]);
            ("costly3", [ ("done", 0.9, Leaves []) ]);
            ("fine", [ ("done", 0.9, Leaves []) ]) ]
        in
        let result, order = run ~strategy:(Search_kernel.Breadth_first { width = 100 }) world [ "g" ] in
        (* Once "dead" has failed, its sibling "costly" cannot help: it is not expanded further. *)
        equal_strings "the sibling of a dead goal is dropped" [ "g"; "dead"; "fine" ] order;
        equal_strings "script" [ "other"; "done" ] (Option.get result.Search_kernel.script);
        let tree = result.Search_kernel.tree in
        check "dead is failed" ((Search_tree.node tree 2).Search_tree.status = Search_tree.Failed);
        check "costly is no longer wanted" (not (Search_tree.relevant tree 3)));
    case "P01-01-08 failure reaches the root when nothing is left" (fun () ->
        let world = [ ("g", [ ("split", 0.9, Leaves [ "a"; "b" ]) ]); ("a", [ ("done", 0.9, Leaves []) ]); ("b", [ ("no", 0.9, Fails) ]) ] in
        let result, _ = run world [ "g" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted);
        check "the root is failed" ((Search_tree.node result.Search_kernel.tree 0).Search_tree.status = Search_tree.Failed);
        check "no script" (result.Search_kernel.script = None));
    case "P01-01-08 a goal with no tactic at all is a dead end" (fun () ->
        let result, _ = run [ ("g", []) ] [ "g" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted)) ]

let repeat_tests =
  [ case "P01-03-04 a tactic that returns to a goal above is blocked" (fun () ->
        let world =
          [ ("g", [ ("forth", 0.9, Leaves [ "h" ]) ]); ("h", [ ("back", 0.9, Leaves [ "g" ]); ("done", 0.1, Leaves []) ]) ]
        in
        let result, order = run world [ "g" ] in
        equal_strings "no goal is expanded twice" [ "g"; "h" ] order;
        equal_strings "script" [ "forth"; "done" ] (Option.get result.Search_kernel.script);
        (match (Search_tree.node result.Search_kernel.tree 2).Search_tree.edges with
         | { Search_tree.tactic = "back"; outcome = Search_tree.Blocked { node = 1 }; _ } :: _ -> ()
         | _ -> fail "the edge back to g is blocked and names g"));
    case "P01-03-04 a circle with no way out ends the search" (fun () ->
        let world = [ ("g", [ ("forth", 0.9, Leaves [ "h" ]) ]); ("h", [ ("back", 0.9, Leaves [ "g" ]) ]) ] in
        let result, order = run world [ "g" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted);
        equal_int "two expansions" 2 (List.length order));
    case "P01-03-05 a tactic that leaves the goal as it was is blocked" (fun () ->
        let world = [ ("g", [ ("skip", 0.9, Leaves [ "g" ]); ("split", 0.05, Leaves [ "g"; "a" ]); ("done", 0.05, Leaves []) ]) ] in
        let result, order = run world [ "g" ] in
        equal_strings "expanded once" [ "g" ] order;
        equal_strings "script" [ "done" ] (Option.get result.Search_kernel.script);
        (match (Search_tree.node result.Search_kernel.tree 1).Search_tree.edges with
         | [ { Search_tree.outcome = Search_tree.Blocked { node = 1 }; _ }; { Search_tree.outcome = Search_tree.Blocked { node = 1 }; _ }; _ ] -> ()
         | _ -> fail "both tactics that leave g behind are blocked, and no node is made for their other subgoals");
        equal_int "no node for a" 2 (Search_tree.size result.Search_kernel.tree));
    case "P01-01-09 a goal reached in two ways is searched once" (fun () ->
        (* Both tactics lead to "shared", which takes three steps to close. *)
        let world =
          [ ("g", [ ("one", 0.5, Leaves [ "a" ]); ("two", 0.5, Leaves [ "b" ]) ]);
            ("a", [ ("to", 0.9, Leaves [ "shared" ]) ]);
            ("b", [ ("to", 0.9, Leaves [ "shared" ]) ]);
            ("shared", [ ("s1", 0.9, Leaves [ "s2" ]) ]);
            ("s2", [ ("s2", 0.9, Leaves [ "s3" ]) ]);
            ("s3", [ ("done", 0.9, Leaves []) ]) ]
        in
        let result, order = run ~strategy:(Search_kernel.Breadth_first { width = 100 }) world [ "g" ] in
        equal_int "shared is expanded once" 1 (List.length (List.filter (( = ) "shared") order));
        let tree = result.Search_kernel.tree in
        let aliases = List.filter (fun id -> (Search_tree.node tree id).Search_tree.alias_of <> None) (List.init (Search_tree.size tree) Fun.id) in
        equal_int "the second arrival is an alias" 1 (List.length aliases);
        check "the alias is proved with its node" (Search_tree.effective tree (List.hd aliases) = Search_tree.Proved);
        check "the script replays" (replays world [ "g" ] (Option.get result.Search_kernel.script)));
    case "P01-01-09 a proof through an alias is written out in full" (fun () ->
        let world =
          [ ("g", [ ("split", 0.9, Leaves [ "a"; "b" ]) ]);
            ("a", [ ("to", 0.9, Leaves [ "shared" ]) ]);
            ("b", [ ("to", 0.9, Leaves [ "shared" ]) ]);
            ("shared", [ ("done", 0.9, Leaves []) ]) ]
        in
        let result, order = run world [ "g" ] in
        equal_strings "order" [ "g"; "a"; "b"; "shared" ] order;
        equal_strings "script" [ "split"; "\xC2\xB7 to"; "  done"; "\xC2\xB7 to"; "  done" ] (Option.get result.Search_kernel.script));
    case "P01-01-09 a failure that depends on the place is not shared" (fun () ->
        (* Under a, the goal x can only go back to a, so it fails there. Under b the same goal is
           fine: a is not above it, and a has a proof of its own. *)
        let world =
          [ ("g", [ ("first", 0.9, Leaves [ "a" ]); ("second", 0.1, Leaves [ "b" ]) ]);
            ("a", [ ("to x", 0.9, Leaves [ "x" ]); ("hard", 0.01, Leaves [ "a2" ]) ]);
            ("a2", [ ("on", 0.5, Leaves [ "a3" ]) ]);
            ("a3", [ ("done", 0.5, Leaves []) ]);
            ("b", [ ("to x", 0.9, Leaves [ "x"; "never" ]); ("via x", 0.5, Leaves [ "x" ]) ]);
            ("x", [ ("to a", 0.9, Leaves [ "a" ]) ]);
            ("never", []) ]
        in
        check "g can be proved" (provable world "g");
        List.iter
          (fun strategy ->
            let result, _ = run ~strategy world [ "g" ] in
            check "found" (result.Search_kernel.stop = Search_kernel.Found);
            check "the script replays" (replays world [ "g" ] (Option.get result.Search_kernel.script)))
          [ Search_kernel.Best_first; Search_kernel.Breadth_first { width = max_int };
            Search_kernel.Depth_first { depth = max_int; branching = max_int } ]);
    case "P01-01-09 a failure that holds everywhere is shared" (fun () ->
        let world =
          [ ("g", [ ("one", 0.5, Leaves [ "a" ]); ("two", 0.5, Leaves [ "b" ]) ]);
            ("a", [ ("to", 0.9, Leaves [ "hopeless" ]) ]);
            ("b", [ ("to", 0.9, Leaves [ "hopeless" ]) ]);
            ("hopeless", [ ("x", 0.5, Fails); ("y", 0.5, Fails) ]) ]
        in
        let result, order = run ~strategy:(Search_kernel.Breadth_first { width = 100 }) world [ "g" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted);
        equal_int "hopeless is expanded once" 1 (List.length (List.filter (( = ) "hopeless") order)));
    case "P01-01-09 goals that wait on each other end the search without a verdict of proof" (fun () ->
        let world =
          [ ("g", [ ("split", 0.9, Leaves [ "x"; "y" ]) ]);
            ("x", [ ("to", 0.9, Leaves [ "p" ]) ]);
            ("y", [ ("to", 0.9, Leaves [ "q" ]) ]);
            ("p", [ ("to", 0.9, Leaves [ "q" ]) ]);
            ("q", [ ("to", 0.9, Leaves [ "p" ]) ]) ]
        in
        let result, _ = run world [ "g" ] in
        check "exhausted" (result.Search_kernel.stop = Search_kernel.Exhausted);
        check "no script" (result.Search_kernel.script = None)) ]

(* Random problem spaces over a few goals. *)
let world_gen : world Gen.t =
 fun state ->
  let goals = List.init (Gen.int 2 7 state) (fun i -> "g" ^ string_of_int i) in
  List.map
    (fun goal ->
      ( goal,
        List.init (Gen.int 0 3 state) (fun i ->
            let move =
              if Gen.int 0 5 state = 0 then Fails
              else Leaves (List.init (Gen.int 0 2 state) (fun _ -> Gen.one_of goals state))
            in
            (Printf.sprintf "t%d" i, Gen.float 0.05 1.0 state, move)) ))
    goals

let show_world (world : world) =
  String.concat "; "
    (List.map
       (fun (goal, tactics) ->
         goal ^ ": "
         ^ String.concat ", "
             (List.map (fun (t, _, m) -> t ^ (match m with Fails -> " fails" | Leaves gs -> " -> [" ^ String.concat " " gs ^ "]")) tactics))
       world)

let law_tests =
  let strategies =
    [ Search_kernel.Best_first; Search_kernel.Breadth_first { width = max_int }; Search_kernel.Depth_first { depth = max_int; branching = max_int } ]
  in
  [ property ~count:500 ~show:show_world "P01-01-07 a script that is returned proves the goal, and the tree follows its rules" world_gen
      (fun world ->
        List.for_all
          (fun strategy ->
            let result, _ = run ~strategy world [ "g0" ] in
            match result.Search_kernel.script with
            | Some script -> result.Search_kernel.stop = Search_kernel.Found && replays world [ "g0" ] script
            | None -> result.Search_kernel.stop <> Search_kernel.Found)
          strategies);
    property ~count:500 ~show:show_world "P01-01-08 nothing that cannot be proved is ever reported as found" world_gen (fun world ->
        let can = provable world "g0" in
        List.for_all (fun strategy -> can || (fst (run ~strategy world [ "g0" ])).Search_kernel.stop <> Search_kernel.Found) strategies);
    property ~count:2000 ~show:show_world "P01-01-09 with no limit, every strategy finds a proof exactly when there is one" world_gen
      (fun world ->
        (* Blocking circles loses no proof, since a proof that returns to a goal contains a shorter
           one; and sharing a goal between two places loses none, since a failure that depends on
           the place is not shared. *)
        let can = provable world "g0" in
        List.for_all (fun strategy -> ((fst (run ~strategy world [ "g0" ])).Search_kernel.stop = Search_kernel.Found) = can) strategies);
    property ~count:300 ~show:show_world "P01-01-02 equal inputs give the same tree" world_gen (fun world ->
        let first, order1 = run world [ "g0" ] and second, order2 = run world [ "g0" ] in
        order1 = order2 && first.Search_kernel.script = second.Search_kernel.script
        && Search_tree.size first.Search_kernel.tree = Search_tree.size second.Search_kernel.tree) ]

(* A simulated prover. A goal is its target; a rule says what a tactic leaves of a target. States
   and goals get fresh numbers, and a tactic changes only the goal it is run on, as in the Bridge. *)
module Sim = struct
  open Maithic_bridge

  type t = {
    rules : ((string * string) * string list) list;
    states : (int, Bridge_types.goal list) Hashtbl.t;
    mutable next_state : int;
    mutable next_goal : int;
    mutable calls : (int * int * string list) list;
    mutable released : int list;
  }

  let goal t ?(hypotheses = []) target =
    t.next_goal <- t.next_goal + 1;
    { Bridge_types.goal_id = Printf.sprintf "_uniq.%d" t.next_goal; tag = ""; hypotheses; target; pretty = "" }

  let create rules targets =
    let t = { rules; states = Hashtbl.create 16; next_state = 0; next_goal = 0; calls = []; released = [] } in
    Hashtbl.replace t.states 0 (List.map (goal t) targets);
    t

  let goals t state = Hashtbl.find t.states state

  let driver t =
    let run state index tactic =
      let before = goals t state in
      let focus = List.nth before index in
      let stepped after =
        t.next_state <- t.next_state + 1;
        Hashtbl.replace t.states t.next_state after;
        Bridge_types.Stepped { state = t.next_state; goals = after; solved = after = []; diagnostics = [] }
      in
      if tactic = "skip" then stepped before
      else if tactic = "#eval 1" then
        Bridge_types.Stopped { kind = Bridge_types.Refused_by_guard; error = "evaluation is not allowed"; diagnostics = [] }
      else if tactic = "(" then Bridge_types.Stopped { kind = Bridge_types.Parse_failure; error = "unexpected end of input"; diagnostics = [] }
      else
        match List.assoc_opt (focus.Bridge_types.target, tactic) t.rules with
        | None -> Bridge_types.Stopped { kind = Bridge_types.Tactic_failure; error = tactic ^ " failed"; diagnostics = [] }
        | Some targets ->
          let fresh = List.map (goal t) targets in
          stepped (List.filteri (fun i _ -> i < index) before @ fresh @ List.filteri (fun i _ -> i > index) before)
    in
    { Lean_search.try_tactics =
        (fun ~state ~goal tactics ->
          t.calls <- t.calls @ [ (state, goal, tactics) ];
          Ok (List.map (run state goal) tactics));
      release = (fun state -> t.released <- state :: t.released) }
end

let conjunction_rules =
  [ (("p \xE2\x88\xA7 q", "constructor"), [ "p"; "q" ]); (("p", "exact hp"), []); (("q", "exact hq"), []) ]

let lean_tests =
  let open Maithic_bridge in
  let found what = function Ok (f : Lean_search.found) -> f | Error e -> fail (what ^ ": " ^ Bridge_error.to_string e) in
  let words = Lean_search.vocabulary [ ("skip", 0.9); ("exact hp", 0.5); ("exact hq", 0.5); ("constructor", 0.4) ] in
  [ case "P01-01-06 the goals a tactic leaves are proved one by one, each under its bullet" (fun () ->
        let sim = Sim.create conjunction_rules [ "p \xE2\x88\xA7 q" ] in
        let result = found "search" (Lean_search.search ~proposer:words (Sim.driver sim) ~state:0 (Sim.goals sim 0)) in
        check "found" (result.Lean_search.stop = Search_kernel.Found);
        equal_string "proof" "by\n  constructor\n  \xC2\xB7 exact hp\n  \xC2\xB7 exact hq" (Option.get result.Lean_search.proof);
        equal_int "three goals were expanded" 3 result.Lean_search.expansions;
        equal_int "one call to Lean for each" 3 result.Lean_search.lean_calls);
    case "P01-01-06 a tactic is run on its own goal, at the position the goal has in the state" (fun () ->
        let sim = Sim.create conjunction_rules [ "p \xE2\x88\xA7 q" ] in
        ignore (found "search" (Lean_search.search ~proposer:words (Sim.driver sim) ~state:0 (Sim.goals sim 0)));
        (match sim.Sim.calls with
         | [ (0, 0, _); (after, 0, _); (after', 1, _) ] ->
           check "both subgoals are searched from the state the tactic left" (after = after' && after <> 0)
         | _ -> fail "three calls: the goal, then its first and its second subgoal"));
    case "P01-01-06 goals that were in the state before are not taken for subgoals" (fun () ->
        (* Two goals at the start. Closing the first leaves the second in the state; it must not
           be counted as something the tactic produced. *)
        let sim = Sim.create conjunction_rules [ "p"; "q" ] in
        let result = found "search" (Lean_search.search ~proposer:words (Sim.driver sim) ~state:0 (Sim.goals sim 0)) in
        equal_string "proof" "by\n  \xC2\xB7 exact hp\n  \xC2\xB7 exact hq" (Option.get result.Lean_search.proof);
        equal_int "two expansions" 2 result.Lean_search.expansions);
    case "P01-03-05 a tactic that leaves its goal in the state has made no progress" (fun () ->
        let sim = Sim.create conjunction_rules [ "p" ] in
        let result = found "search" (Lean_search.search ~proposer:(Lean_search.vocabulary [ ("skip", 0.9) ]) (Sim.driver sim) ~state:0 (Sim.goals sim 0)) in
        check "skip proves nothing" (result.Lean_search.stop = Search_kernel.Exhausted && result.Lean_search.proof = None);
        equal_int "one expansion" 1 result.Lean_search.expansions);
    case "P01-01-08 what stops a tactic is kept by its kind" (fun () ->
        let sim = Sim.create conjunction_rules [ "p" ] in
        let node =
          let tree = Search_tree.create () in
          let goals = Sim.goals sim 0 in
          Search_tree.node tree (Search_tree.add_root tree ~key:"p" { Lean_search.state = 0; goals; goal = Some (List.hd goals) })
        in
        let expansion =
          match Lean_search.expand (Sim.driver sim) (Lean_search.vocabulary [ ("#eval 1", 0.5); ("(", 0.5); ("rfl", 0.5); ("exact hp", 0.5) ]) node with
          | Ok e -> e
          | Error e -> fail (Bridge_error.to_string e)
        in
        equal_strings "kinds" [ "guard"; "parse"; "tactic"; "closed" ]
          (List.map
             (fun (tried : _ Search_kernel.tried) ->
               match tried.Search_kernel.step with
               | Search_tree.Fails { error_class; _ } -> error_class
               | Search_tree.Leaves [] -> "closed"
               | Search_tree.Leaves _ -> "subgoals")
             expansion.Search_kernel.tried));
    case "P01-01-05 the states of a search are released, and the state it started from is not" (fun () ->
        let sim = Sim.create conjunction_rules [ "p \xE2\x88\xA7 q" ] in
        let result = found "search" (Lean_search.search ~proposer:words (Sim.driver sim) ~state:0 (Sim.goals sim 0)) in
        check "the first state is kept" (not (List.mem 0 sim.Sim.released));
        (* constructor left one state; exact hp and exact hq closed their goals and left one each,
           although the search did not go on from them; skip left one at every goal it was tried on. *)
        equal_int "every state that a tactic left is released, once" sim.Sim.next_state (List.length sim.Sim.released);
        equal_int "and counted" sim.Sim.next_state result.Lean_search.states;
        check "none twice" (List.length (List.sort_uniq compare sim.Sim.released) = List.length sim.Sim.released));
    case "P01-01-05 the default budget bounds a search that finds nothing" (fun () ->
        (* Every tactic leads to a new goal, for ever. *)
        let endless = List.init 500 (fun i -> ((string_of_int i, "next"), [ string_of_int (i + 1) ])) in
        let sim = Sim.create endless [ "0" ] in
        let result = found "search" (Lean_search.search ~proposer:(Lean_search.vocabulary [ ("next", 0.9) ]) (Sim.driver sim) ~state:0 (Sim.goals sim 0)) in
        check "out of expansions" (result.Lean_search.stop = Search_kernel.Out_of "expansions");
        equal_int "sixty" 60 result.Lean_search.expansions;
        equal_int "the budget" 60 Lean_search.default_budget.Search_kernel.expansions);
    case "P01-01-09 the key of a goal is its target and hypotheses, whatever their order" (fun () ->
        let h name ty = { Bridge_types.hyp_name = name; hyp_type = ty; hyp_value = None } in
        let g id tag hypotheses = { Bridge_types.goal_id = id; tag; hypotheses; target = "x = y"; pretty = "" } in
        equal_string "identifier, case name and order do not matter"
          (Lean_search.goal_key (g "a" "" [ h "x" "Nat"; h "hx" "x > 0" ]))
          (Lean_search.goal_key (g "b" "succ" [ h "hx" "x > 0"; h "x" "Nat" ]));
        check "names matter" (Lean_search.goal_key (g "a" "" [ h "x" "Nat" ]) <> Lean_search.goal_key (g "a" "" [ h "z" "Nat" ]));
        check "values of let variables matter"
          (Lean_search.goal_key (g "a" "" [ { (h "x" "Nat") with Bridge_types.hyp_value = Some "3" } ]) <> Lean_search.goal_key (g "a" "" [ h "x" "Nat" ]));
        equal_string "text" "hx : x > 0\nx : Nat\n\xE2\x8A\xA2 x = y" (Lean_search.goal_key (g "a" "" [ h "x" "Nat"; h "hx" "x > 0" ])));
    case "P01-02-01 the proposer without a model picks tactics from the shape of the goal" (fun () ->
        let h name ty = { Bridge_types.hyp_name = name; hyp_type = ty; hyp_value = None } in
        let tactics hypotheses target =
          List.map fst (Lean_search.shape_proposer { Bridge_types.goal_id = "g"; tag = ""; hypotheses; target; pretty = "" })
        in
        let closing = [ "rfl"; "assumption"; "omega"; "simp"; "simp_all"; "decide"; "grind" ] in
        let props = [ h "p" "Prop"; h "q" "Prop" ] in
        equal_strings "automation on every goal" closing (tactics [ h "a" "Int" ] "a = a");
        equal_strings "a conjunction is split" (closing @ [ "constructor" ]) (tactics props "p \xE2\x88\xA7 q");
        equal_strings "an implication is introduced" (closing @ [ "intros" ]) (tactics props "p \xE2\x86\x92 q");
        equal_strings "a disjunction is tried on both sides" (closing @ [ "left"; "right" ]) (tactics props "p \xE2\x88\xA8 q");
        equal_strings "hypotheses that can be taken apart, and variables to induct on"
          (closing @ [ "cases h"; "induction n"; "induction xs" ])
          (tactics [ h "h" "a \xE2\x88\xA7 b"; h "n" "Nat"; h "xs" "List Nat"; h "k" "Int" ] "n = n");
        equal_strings "a name Lean made up cannot be referred to" closing
          (tactics [ h "a\xE2\x9C\x9D" "a \xE2\x88\xA7 b"; h "x" "Int" ] "x = x");
        equal_strings "a name of the target that is no variable may be a definition to unfold"
          (closing @ [ "induction n"; "simp [double]" ])
          (tactics [ h "n" "Nat" ] "double (n + 1) = 2 * (n + 1)");
        equal_strings "the first two such names, each once, and no word of the language"
          (closing @ [ "simp [f]"; "simp [g]" ])
          (tactics [] "f (g (f (k 1))) = if True then 1 else 2");
        equal_strings "the truth values are not definitions" (closing @ [ "simp [isEven]" ]) (tactics [] "(isEven 4 || false) = true");
        check "priors are in (0, 1]"
          (List.for_all (fun (_, prior) -> prior > 0.0 && prior <= 1.0)
             (Lean_search.shape_proposer { Bridge_types.goal_id = "g"; tag = ""; hypotheses = [ h "h" "a \xE2\x88\xA8 b" ]; target = "\xE2\x88\x80 x, p x"; pretty = "" })));
    case "P01-01-07 a script becomes a proof term" (fun () ->
        equal_string "by and indentation" "by\n  constructor\n  \xC2\xB7 rfl\n  \xC2\xB7 simp"
          (Lean_search.script_to_proof [ "constructor"; "\xC2\xB7 rfl"; "\xC2\xB7 simp" ])) ]

(* ---- the record of a search, and its picture ---- *)

let describe (node : string Search_tree.node) = (node.Search_tree.payload, if node.Search_tree.payload = "" then [] else [ "h : " ^ node.Search_tree.payload ])

let record_of ?now ?strategy ?budget world goals =
  let result, order = run ?now ?strategy ?budget world goals in
  (Search_record.of_result ~statement:"theorem t : g" ~describe result, order)

let count sub text =
  let rec go from acc = match Text.find_sub ~from text sub with Some i -> go (i + String.length sub) (acc + 1) | None -> acc in
  go 0 0

(* Whether the tags of a document open and close in order. *)
let well_formed text =
  let n = String.length text in
  let rec go i stack =
    if i >= n then stack = []
    else if text.[i] <> '<' then go (i + 1) stack
    else
      match String.index_from_opt text i '>' with
      | None -> false
      | Some close ->
        let inside = String.sub text (i + 1) (close - i - 1) in
        let name s = List.hd (String.split_on_char ' ' s) in
        if inside <> "" && inside.[0] = '/' then
          (match stack with top :: rest when top = String.sub inside 1 (String.length inside - 1) -> go (close + 1) rest | _ -> false)
        else if inside <> "" && inside.[String.length inside - 1] = '/' then go (close + 1) stack
        else go (close + 1) (name inside :: stack)
  in
  go 0 []

let search_schema = lazy (is_ok "the schema" (Json_schema.load (Support.path "schemas/prove/search.schema.json")))
let spin = [ ("g", [ ("spin", 0.9, Leaves [ "g" ]); ("close", 0.5, Leaves []) ]) ]
let dead = [ ("g", [ ("try", 0.9, Leaves [ "h" ]); ("close", 0.1, Fails) ]); ("h", [ ("no", 0.5, Fails) ]) ]
let recorded = [ "found"; "stopped" ]

let view_tests =
  [ case "P01-06-03 a search is one document: the tree, the steps and the proof" (fun () ->
        let record, order = record_of split [ "g" ] in
        let json = Search_record.to_json record in
        check "it conforms to its schema" (Json_schema.conforms (Lazy.force search_schema) json = Ok ());
        equal_string "it names its schema" "maithic.prove.search/1" (Json.string_or "" "schema" json);
        equal_int "every node of the tree, and the node for all goals" 7 (List.length (Json.list_or_empty "nodes" json));
        equal_int "a step for every expansion" (List.length order) (List.length (Json.list_or_empty "steps" json));
        equal_strings "the steps are the expansions in their order" order
          (List.map
             (fun (s : Search_record.step) -> (Option.get (Search_record.node record s.Search_record.node)).Search_record.goal)
             record.Search_record.steps);
        equal_strings "the proof"
          [ "constructor"; "\xC2\xB7 exact hp"; "\xC2\xB7 cases h"; "  \xC2\xB7 simp"; "    rfl"; "  \xC2\xB7 omega" ]
          (Json.strings (Option.get (Json.member "script" json)));
        equal_string "how it ended" "found" record.Search_record.stop;
        equal_ints "the goals the search began with" [ 1 ] (Search_record.first_goals record);
        check "the document reads back as the record" (Search_record.of_json json = Ok record);
        check "also from its text" (Result.bind (Json.parse (Json.pretty json)) Search_record.of_json = Ok record));
    case "P01-06-03 searches that fail or stop are recorded with what was left open" (fun () ->
        let failed, _ = record_of dead [ "g" ] in
        equal_string "exhausted" "exhausted" failed.Search_record.stop;
        check "no proof" (failed.Search_record.script = None && Json.member "script" (Search_record.to_json failed) = None);
        check "both goals failed"
          (List.for_all
             (fun (n : Search_record.node) -> n.Search_record.goal = "" || n.Search_record.status = Search_tree.Failed)
             failed.Search_record.nodes);
        check "the document conforms" (Json_schema.conforms (Lazy.force search_schema) (Search_record.to_json failed) = Ok ());
        let endless = List.init 30 (fun i -> (string_of_int i, [ ("next", 0.9, Leaves [ string_of_int (i + 1) ]) ])) in
        let stopped, _ = record_of ~budget:{ Search_kernel.unlimited with Search_kernel.expansions = 3 } endless [ "0" ] in
        equal_string "out of expansions" "out of expansions" stopped.Search_record.stop;
        equal_int "three steps" 3 (List.length stopped.Search_record.steps);
        check "the last goal was never expanded"
          (match List.rev stopped.Search_record.nodes with
           | last :: _ -> (not last.Search_record.expanded) && last.Search_record.status = Search_tree.Open
           | [] -> false);
        check "it reads back" (Search_record.of_json (Search_record.to_json stopped) = Ok stopped);
        let limited, _ = record_of ~strategy:(Search_kernel.Depth_first { depth = 2; branching = 1 }) endless [ "0" ] in
        check "a goal given up says why"
          (List.exists (fun (n : Search_record.node) -> n.Search_record.abandoned = Some "depth limit") limited.Search_record.nodes);
        check "and that is kept in the document" (Search_record.of_json (Search_record.to_json limited) = Ok limited));
    case "P01-06-03 a record that does not hold together is refused" (fun () ->
        let record, _ = record_of chain [ "g" ] in
        let json = Search_record.to_json record in
        let set name value = match json with Json.Obj fields -> Json.Obj (List.map (fun (k, v) -> if k = name then (k, value) else (k, v)) fields) | j -> j in
        let refused what j = check what (Result.is_error (Search_record.of_json j)) in
        refused "another schema" (set "schema" (Json.String "maithic.prove.run/1"));
        refused "a root that is not there" (set "root" (Json.Int 99));
        refused "a step at a node that is not there" (set "steps" (Json.List [ Json.Obj [ ("node", Json.Int 7); ("started_ms", Json.Int 0); ("elapsed_ms", Json.Int 0) ] ]));
        let nodes = Json.list_or_empty "nodes" json in
        let patch position f = set "nodes" (Json.List (List.mapi (fun i n -> if i = position then f n else n) nodes)) in
        let field name value = function Json.Obj fields -> Json.Obj ((name, value) :: List.remove_assoc name fields) | j -> j in
        refused "a node out of its place" (patch 1 (field "id" (Json.Int 2)));
        refused "a proof by an edge that is not there" (patch 1 (field "proved_by" (Json.Int 5)));
        refused "a parent that is not there" (patch 2 (field "parent" (Json.Int 40)));
        refused "a status that does not exist" (patch 2 (field "status" (Json.String "maybe")));
        refused "a subgoal that is not there"
          (patch 1
             (field "edges"
                (Json.List [ Json.Obj [ ("tactic", Json.String "step"); ("prior", Json.Float 0.9); ("outcome", Json.String "subgoals"); ("subgoals", Json.List [ Json.Int 12 ]) ] ])));
        check "the record itself is accepted" (Search_record.of_json json = Ok record));
    case "P01-06-02 the steps are listed in order, with what each tactic did and how long it took" (fun () ->
        (* A clock that advances a quarter of a second each time it is read. *)
        let ticks = ref 0.0 in
        let now () = ticks := !ticks +. 0.25; !ticks in
        let record, _ = record_of ~now chain [ "g" ] in
        equal_ints "each expansion took one reading of the clock" [ 250; 250 ]
          (List.map (fun (s : Search_record.step) -> s.Search_record.elapsed_ms) record.Search_record.steps);
        check "the steps begin one after the other"
          (match record.Search_record.steps with [ a; b ] -> a.Search_record.started_ms + a.Search_record.elapsed_ms <= b.Search_record.started_ms | _ -> false);
        check "the search took longer than its steps" (record.Search_record.elapsed_ms >= 1000);
        equal_string "the listing"
          "step 1  goal #1  g  (250 ms)\n  * step   leaves 1 goal: #2\nstep 2  goal #2  h  (250 ms)\n    fail   fails (tactic)\n  * close  closes the goal\n"
          (Search_record.steps_text record);
        let circle, _ = record_of spin [ "g" ] in
        check "a tactic that leads in a circle says where to"
          (Text.contains (Search_record.steps_text circle) "    spin   leads back to #1\n  * close  closes the goal");
        let both, _ = record_of split [ "g" ] in
        check "several goals are counted and named" (Text.contains (Search_record.steps_text both) "  * constructor  leaves 2 goals: #2, #3");
        equal_string "one line says how it ended and what it cost" "found: 2 goals expanded, 2 calls to Lean, 3 nodes, 1750 ms"
          (Search_record.summary record));
    case "P01-06-01 the picture shows every goal with its status and every tactic that left goals" (fun () ->
        let record, _ = record_of split [ "g" ] in
        let svg = Search_svg.render record in
        check "the tags open and close in order" (well_formed svg);
        check "it is an image with a title and a description"
          (Text.contains svg "role=\"img\"" && Text.contains svg "<title id=\"t\">The search for a proof of theorem t : g</title>"
           && Text.contains svg "The proof that was found: constructor;");
        equal_int "six goals, all proved" 6 (count "<g class=\"goal proved\">" svg);
        equal_int "six tactics, all of the proof" 6 (count "<g class=\"tactic proof\">" svg);
        List.iter
          (fun tactic -> check ("the tactic " ^ tactic) (Text.contains svg (">" ^ tactic ^ "</text>")))
          [ "constructor"; "cases h"; "simp"; "exact hp \xE2\x9C\x93"; "rfl \xE2\x9C\x93"; "omega \xE2\x9C\x93" ];
        check "what is to be shown, and the hypotheses" (Text.contains svg ">\xE2\x8A\xA2 right</text>" && Text.contains svg ">h : right</text>");
        check "the proof is written out" (Text.contains svg "THE PROOF" && Text.contains svg ">  \xC2\xB7 exact hp</text>");
        check "what it cost" (Text.contains svg "found \xC2\xB7 6 goals expanded \xC2\xB7 6 calls to Lean \xC2\xB7 6 goals drawn"));
    case "P01-06-01 open, failed and repeated goals are told apart" (fun () ->
        let found, _ = record_of fork [ "g" ] in
        let svg = Search_svg.render found in
        equal_int "three goals of the proof" 3 (count "<g class=\"goal proved\">" svg);
        equal_int "one goal that was never expanded, drawn small" 1 (count "<g class=\"goal open small\">" svg);
        check "it says so" (Text.contains svg ">not expanded</text>");
        equal_int "three tactics of the proof" 3 (count "<g class=\"tactic proof\">" svg);
        equal_int "one tactic beside it" 1 (count "<g class=\"tactic\">" svg);
        let failed, _ = record_of dead [ "g" ] in
        let svg = Search_svg.render failed in
        equal_int "two failed goals" 2 (count "<g class=\"goal failed\">" svg);
        check "the tactics that failed are named on the goal" (Text.contains svg ">\xE2\x9C\x97 close</text>" && Text.contains svg ">\xE2\x9C\x97 no</text>");
        check "no proof" (Text.contains svg "No proof was found." && not (Text.contains svg "THE PROOF"));
        let circle, _ = record_of spin [ "g" ] in
        check "a tactic that leads in a circle is marked" (Text.contains (Search_svg.render circle) ">\xE2\x86\xBA spin</text>");
        let twice, _ = record_of [ ("g", [ ("both", 0.9, Leaves [ "x"; "x" ]) ]); ("x", [ ("close", 0.9, Leaves []) ]) ] [ "g" ] in
        let svg = Search_svg.render twice in
        check "a goal met twice is drawn once and referred to" (Text.contains svg ">the same goal as #2</text>" && Text.contains svg "stroke-dasharray");
        let several, _ = record_of split [ "left"; "r2" ] in
        check "several goals at the start hang under one node" (Text.contains (Search_svg.render several) ">all goals</text>"));
    case "P01-06-01 text is escaped, long trees are cut, and equal searches give equal pictures" (fun () ->
        let odd = "a < b & \"c\"" in
        let record, _ = record_of [ (odd, [ ("close", 0.9, Leaves []) ]) ] [ odd ] in
        let svg = Search_svg.render record in
        check "well formed with marks in the text" (well_formed svg);
        check "the marks are written as entities" (Text.contains svg "\xE2\x8A\xA2 a &lt; b &amp; &quot;c&quot;");
        let endless = List.init 30 (fun i -> (string_of_int i, [ ("next", 0.9, Leaves [ string_of_int (i + 1) ]) ])) in
        let long, _ = record_of ~budget:{ Search_kernel.unlimited with Search_kernel.expansions = 12 } endless [ "0" ] in
        let cut = Search_svg.render ~max_nodes:5 long in
        equal_int "five goals are drawn" 5 (count "<g class=\"goal " cut);
        check "the tactic whose goal is left out says so" (Text.contains cut ">next (1 not drawn)</text>");
        equal_int "all of them otherwise" 13 (count "<g class=\"goal " (Search_svg.render long));
        let ticks = ref 0.0 in
        let slow () = ticks := !ticks +. 3.0; !ticks in
        let again, _ = record_of ~now:slow split [ "g" ] and once, _ = record_of split [ "g" ] in
        check "the timings differ" (again.Search_record.elapsed_ms <> once.Search_record.elapsed_ms);
        equal_string "and the pictures do not" (Search_svg.render once) (Search_svg.render again));
    case "P01-06-04 the pictures of the recorded searches are the ones in the repository" (fun () ->
        List.iter
          (fun name ->
            let json = Support.json ("fixtures/trees/" ^ name ^ "/double_eq.json") in
            check (name ^ ": the record conforms") (Json_schema.conforms (Lazy.force search_schema) json = Ok ());
            let record = is_ok name (Search_record.of_json json) in
            equal_string (name ^ ": the picture") (Support.read ("fixtures/trees/" ^ name ^ "/double_eq.svg")) (Search_svg.render record))
          recorded;
        let found = is_ok "found" (Search_record.of_json (Support.json "fixtures/trees/found/double_eq.json")) in
        equal_string "the search that found the proof" "found" found.Search_record.stop;
        equal_strings "its proof" [ "induction n"; "\xC2\xB7 rfl"; "\xC2\xB7 simp [double]"; "  omega" ] (Option.value ~default:[] found.Search_record.script);
        let stopped = is_ok "stopped" (Search_record.of_json (Support.json "fixtures/trees/stopped/double_eq.json")) in
        equal_string "the search that ran out" "out of expansions" stopped.Search_record.stop;
        equal_string "the picture in the README is the picture of the search that found the proof"
          (Support.read "fixtures/trees/found/double_eq.svg") (Support.read "assets/readme/search-tree.svg")) ]

let () =
  Testkit.run "search"
    [ ("tree", tree_tests); ("strategies", strategy_tests); ("budgets", budget_tests); ("and-or", and_or_tests);
      ("repeats", repeat_tests); ("laws", law_tests); ("lean", lean_tests); ("view", view_tests) ]
