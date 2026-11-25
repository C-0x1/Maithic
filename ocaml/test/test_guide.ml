(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the guidance client, of the fallback and of the priors in the search. The service is
   stood in for by a function, and Lean by a table of rules; the real ones meet in the
   integration tests. *)

open Maithic_base
open Maithic_bridge
open Maithic_prover
open Maithic_guide
open Testkit

let close_to what expected actual = check (Printf.sprintf "%s: expected %g, got %g" what expected actual) (Float.abs (expected -. actual) < 1e-9)
let close_list what expected actual =
  check (what ^ ": as many numbers as expected") (List.length expected = List.length actual);
  List.iteri (fun i e -> close_to (Printf.sprintf "%s, entry %d" what i) e (List.nth actual i)) expected

let greeting =
  [ ("service", Json.String "maithic.guide.service/1");
    ( "model",
      Json.Obj
        [ ("dim", Json.Int 32); ("max_length", Json.Int 128); ("pooling", Json.String "mean"); ("mixer", Json.String "none");
          ("vocabulary", Json.Int 380); ("classes", Json.Int 33) ] ) ]

let meth request = Json.string_or "" "method" request
let id_of request = Option.value ~default:Json.Null (Json.member "id" request)
let candidates_of request = match Json.member "candidates" request with Some l -> Json.strings l | None -> []
let floats xs = Json.List (List.map (fun x -> Json.Float x) xs)
let texts xs = Json.List (List.map (fun x -> Json.String x) xs)

(* A service played by a function. It keeps what it was asked, answers under the id of the
   request, and can be told to fail from a given question on. *)
type stand_in = { mutable seen : Json.t list; mutable closed : int; fail_from : int option }

let stand_in ?fail_from () = { seen = []; closed = 0; fail_from }
let questions s = List.filter (fun r -> List.mem (meth r) [ "tactic"; "value"; "premise" ]) (List.rev s.seen)

let transport ?(weight = fun _ -> 1.0) s =
  let exchange request =
    s.seen <- request :: s.seen;
    let asked = List.length (questions s) in
    match s.fail_from with
    | Some n when asked >= n && meth request <> "handshake" -> Error "the pipe is broken"
    | _ ->
      let answer members = Ok (Json.Obj (("id", id_of request) :: members)) in
      (match meth request with
       | "handshake" -> answer greeting
       | "tactic" ->
         let candidates = candidates_of request in
         let weights = List.map weight candidates in
         let total = List.fold_left ( +. ) 0.0 weights in
         answer [ ("priors", floats (List.map (fun w -> w /. total) weights)); ("templates", texts (List.map (fun c -> "<" ^ c ^ ">") candidates)) ]
       | "value" -> answer [ ("value", Json.Float 0.25) ]
       | "premise" -> answer [ ("scores", floats (List.map (fun c -> if c = "Nat.add_comm" then 1.0 else 0.0) (candidates_of request))) ]
       | "shutdown" -> answer [ ("ok", Json.Bool true) ]
       | other -> answer [ ("error", Json.String ("there is no method \"" ^ other ^ "\"")) ])
  in
  { Guide_client.exchange; close = (fun () -> s.closed <- s.closed + 1) }

(* A service that answers every request with the same members, whatever was asked. *)
let fixed members =
  { Guide_client.exchange = (fun request -> Ok (Json.Obj (if List.mem_assoc "id" members then members else ("id", id_of request) :: members)));
    close = ignore }

(* A client whose greeting succeeds and whose later answers are the given members. *)
let answering members =
  let first = ref true in
  let exchange request =
    if !first then begin
      first := false;
      Ok (Json.Obj (("id", id_of request) :: greeting))
    end
    else Ok (Json.Obj (if List.mem_assoc "id" members then members else ("id", id_of request) :: members))
  in
  is_ok "connect" (Guide_client.connect { Guide_client.exchange; close = ignore })

let client_tests =
  [ case "P12-05-01 the client greets the service and learns what model is behind it" (fun () ->
        let s = stand_in () in
        let client = is_ok "connect" (Guide_client.connect (transport s)) in
        check "the greeting is the first request, with the number one"
          (Json.equal (List.hd (List.rev s.seen)) (Json.Obj [ ("id", Json.Int 1); ("method", Json.String "handshake") ]));
        let info = Guide_client.info client in
        equal_int "dim" 32 info.Guide_client.dim;
        equal_int "max_length" 128 info.Guide_client.max_length;
        equal_string "pooling" "mean" info.Guide_client.pooling;
        equal_string "mixer" "none" info.Guide_client.mixer;
        equal_int "vocabulary" 380 info.Guide_client.vocabulary;
        equal_int "classes" 33 info.Guide_client.classes;
        equal_int "one request so far" 1 (Guide_client.requests client);
        equal_string "the protocol" "maithic.guide.service/1" Guide_client.protocol);
    case "P12-05-01 a service that speaks another protocol, or none, is refused and closed" (fun () ->
        let refused members =
          let closed = ref 0 in
          let t = { (fixed members) with Guide_client.close = (fun () -> incr closed) } in
          let why = is_error "refused" (Guide_client.connect t) in
          equal_int "closed" 1 !closed;
          why
        in
        contains "another protocol" (refused [ ("service", Json.String "maithic.guide.service/2") ]) "speaks maithic.guide.service/2, and this client speaks maithic.guide.service/1";
        contains "no protocol" (refused [ ("hello", Json.Bool true) ]) "is not a guidance service";
        contains "no model" (refused [ ("service", Json.String "maithic.guide.service/1") ]) "did not describe its model";
        contains "half a model" (refused [ ("service", Json.String "maithic.guide.service/1"); ("model", Json.Obj [ ("dim", Json.Int 4) ]) ]) "incomplete";
        contains "a refusal" (refused [ ("error", Json.String "the model could not be loaded") ]) "the model could not be loaded";
        let silent = { Guide_client.exchange = (fun _ -> Error "no answer in 120 s"); close = ignore } in
        equal_string "a service that does not answer" "no answer in 120 s" (is_error "silent" (Guide_client.connect silent)));
    case "P12-05-01 tactic, value and premise are typed calls, each with a number of its own" (fun () ->
        let s = stand_in () in
        let client = is_ok "connect" (Guide_client.connect (transport ~weight:(fun c -> if c = "omega" then 3.0 else 1.0) s)) in
        let state = "n : Nat\n\xE2\x8A\xA2 n + 0 = n" in
        let ranking = is_ok "tactic" (Guide_client.tactic client ~state [ "simp"; "omega" ]) in
        close_list "priors, in the order of the candidates" [ 0.25; 0.75 ] ranking.Guide_client.priors;
        equal_strings "how each candidate was read" [ "<simp>"; "<omega>" ] ranking.Guide_client.templates;
        close_to "value" 0.25 (is_ok "value" (Guide_client.value client ~state));
        close_list "premise scores" [ 0.0; 1.0 ] (is_ok "premise" (Guide_client.premise client ~state [ "List.map_id"; "Nat.add_comm" ]));
        (match questions s with
         | [ first; second; third ] ->
           check "the tactic request"
             (Json.equal first
                (Json.Obj [ ("id", Json.Int 2); ("method", Json.String "tactic"); ("state", Json.String state); ("candidates", texts [ "simp"; "omega" ]) ]));
           check "the value request" (Json.equal second (Json.Obj [ ("id", Json.Int 3); ("method", Json.String "value"); ("state", Json.String state) ]));
           check "the premise request"
             (Json.equal third
                (Json.Obj
                   [ ("id", Json.Int 4); ("method", Json.String "premise"); ("state", Json.String state);
                     ("candidates", texts [ "List.map_id"; "Nat.add_comm" ]) ]))
         | _ -> fail "three questions were expected");
        equal_int "four requests with the greeting" 4 (Guide_client.requests client);
        (* Nothing to rank is not worth a call. *)
        check "no candidates, no priors" (is_ok "empty" (Guide_client.tactic client ~state []) = { Guide_client.priors = []; templates = [] });
        check "no premises, no scores" (is_ok "empty" (Guide_client.premise client ~state []) = []);
        equal_int "and no request" 4 (Guide_client.requests client));
    case "P12-05-01 an answer that does not fit its question is refused" (fun () ->
        let tactic members = is_error "tactic" (Guide_client.tactic (answering members) ~state:"s" [ "a"; "b" ]) in
        let good_templates = ("templates", texts [ "a"; "b" ]) in
        contains "another request" (tactic [ ("id", Json.Int 99); ("priors", floats [ 0.5; 0.5 ]); good_templates ]) "answered another request";
        contains "too few" (tactic [ ("priors", floats [ 1.0 ]); good_templates ]) "gave 1 priors for 2 candidates";
        contains "not adding up" (tactic [ ("priors", floats [ 0.5; 0.25 ]); good_templates ]) "add up to 0.75, not to 1";
        contains "negative" (tactic [ ("priors", floats [ 1.5; -0.5 ]); good_templates ]) "are not probabilities";
        contains "not numbers" (tactic [ ("priors", Json.List [ Json.String "half"; Json.Float 0.5 ]); good_templates ]) "are not numbers";
        contains "missing" (tactic [ good_templates ]) "answered without priors";
        contains "no templates" (tactic [ ("priors", floats [ 0.5; 0.5 ]) ]) "did not say how it read each candidate";
        contains "a refusal" (tactic [ ("error", Json.String "the request needs \"candidates\", a list of texts") ]) "refused the request: the request needs";
        let value members = is_error "value" (Guide_client.value (answering members) ~state:"s") in
        contains "out of range" (value [ ("value", Json.Float 1.5) ]) "1.5, which is not between 0 and 1";
        contains "no value" (value [ ("worth", Json.Float 0.5) ]) "answered without a value";
        let premise members = is_error "premise" (Guide_client.premise (answering members) ~state:"s" [ "a"; "b" ]) in
        contains "scores out of range" (premise [ ("scores", floats [ 0.5; 2.0 ]) ]) "not between 0 and 1";
        contains "too many scores" (premise [ ("scores", floats [ 0.5; 0.5; 0.5 ]) ]) "gave 3 scores for 2 candidates";
        (* Whole numbers are numbers: a service may write 1 for 1.0. *)
        let whole = answering [ ("priors", Json.List [ Json.Int 1; Json.Int 0 ]); good_templates ] in
        close_list "whole numbers" [ 1.0; 0.0 ] (is_ok "whole" (Guide_client.tactic whole ~state:"s" [ "a"; "b" ])).Guide_client.priors);
    case "P12-05-01 closing says goodbye once, and nothing is asked after it" (fun () ->
        let s = stand_in () in
        let client = is_ok "connect" (Guide_client.connect (transport s)) in
        Guide_client.close client;
        equal_string "the last request is the goodbye" "shutdown" (meth (List.hd s.seen));
        equal_int "the transport was closed" 1 s.closed;
        let sent = List.length s.seen in
        contains "a call after closing" (is_error "closed" (Guide_client.value client ~state:"s")) "was closed";
        Guide_client.close client;
        equal_int "nothing more was sent" sent (List.length s.seen);
        equal_int "and the transport was closed once" 1 s.closed);
    case "P12-05-01 the service is an entry point of the Julia package, started on a model file" (fun () ->
        let engine = { Julia_package.julia = "julia"; project = "/src/julia/MaithicGuide" } in
        check "serving"
          (Guide_service.command engine ~model:"m/model.json"
           = ("julia", [ "--startup-file=no"; "--project=/src/julia/MaithicGuide"; "/src/julia/MaithicGuide/bin/serve.jl"; "m/model.json" ]));
        check "training"
          (Guide_service.train_command engine ~dataset:"data" ~out:"out" [ "--seed"; "3" ]
           = ("julia", [ "--startup-file=no"; "--project=/src/julia/MaithicGuide"; "/src/julia/MaithicGuide/bin/train.jl"; "data"; "out"; "--seed"; "3" ]));
        check "an answer may take a while, the first start longer" (Guide_service.default_timeout < Guide_service.default_startup);
        contains "no model, nothing started" (is_error "absent" (Guide_service.start engine ~model:"/nowhere/model.json")) "there is no model at /nowhere/model.json";
        contains "the same from connect" (is_error "absent" (Guide_service.connect engine ~model:"/nowhere/model.json")) "there is no model at";
        let here = Lazy.force Support.root in
        (match Proc.find_in_path "sh" with
         | None -> skip "no shell on the path to stand in for Julia"
         | Some _ ->
           let found = is_ok "locate" (Guide_service.locate ~julia:"sh" ~cwd:Fs.(here / "ocaml" / "test") ()) in
           equal_string "the package of the source tree" Fs.(here / "julia" / "MaithicGuide") found.Julia_package.project;
           contains "another directory" (is_error "nowhere" (Guide_service.locate ~julia:"sh" ~cwd:"/" ())) "the package MaithicGuide was not found";
           contains "what to set" (is_error "nowhere" (Guide_service.locate ~julia:"sh" ~cwd:"/" ())) "set guide.project");
        contains "no Julia" (is_error "julia" (Guide_service.locate ~julia:"no-such-julia" ~cwd:here ())) "is not on the path");
    case "P12-05-01 requests and answers cross a process line by line" (fun () ->
        match Proc.find_in_path "sh" with
        | None -> skip "no shell on the path to stand in for Julia"
        | Some _ ->
          let dir = Fs.temp_dir "maithic-guide" in
          (* A stand-in for Julia: it answers each line under the id of the request, and leaves on a goodbye. *)
          let script =
            String.concat "\n"
              [ "#!/bin/sh"; "while IFS= read -r line; do"; "  id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\\([0-9]*\\).*/\\1/')";
                "  case \"$line\" in";
                "    *handshake*) echo \"{\\\"id\\\":$id,\\\"service\\\":\\\"maithic.guide.service/1\\\",\\\"model\\\":{\\\"dim\\\":8,\\\"max_length\\\":16,\\\"pooling\\\":\\\"mean\\\",\\\"mixer\\\":\\\"none\\\",\\\"vocabulary\\\":9,\\\"classes\\\":3}}\";;";
                "    *tactic*) echo \"{\\\"id\\\":$id,\\\"priors\\\":[0.75,0.25],\\\"templates\\\":[\\\"omega\\\",\\\"simp\\\"]}\";;";
                "    *value*) echo 'this is not JSON';;"; "    *shutdown*) echo \"{\\\"id\\\":$id,\\\"ok\\\":true}\"; exit 0;;"; "  esac"; "done"; "" ]
          in
          let julia = Fs.(dir / "julia-stand-in") in
          ignore (is_ok "write" (Fs.write julia script));
          Unix.chmod julia 0o755;
          ignore (is_ok "manifest" (Fs.write Fs.(dir / "Manifest.toml") ""));
          ignore (is_ok "model" (Fs.write Fs.(dir / "model.json") "{}"));
          let engine = { Julia_package.julia; project = dir } in
          let client = is_ok "connect" (Guide_service.connect ~startup:20.0 ~timeout:20.0 engine ~model:Fs.(dir / "model.json")) in
          equal_int "the model of the greeting" 8 (Guide_client.info client).Guide_client.dim;
          let ranking = is_ok "tactic" (Guide_client.tactic client ~state:"\xE2\x8A\xA2 1 \xE2\x89\xA4 2" [ "omega"; "simp" ]) in
          close_list "priors over the pipe" [ 0.75; 0.25 ] ranking.Guide_client.priors;
          contains "what is not JSON is an error" (is_error "value" (Guide_client.value client ~state:"s")) "wrote something that is not JSON";
          (* After a failure the process is stopped, and it is not asked again. *)
          contains "stopped" (is_error "after" (Guide_client.tactic client ~state:"s" [ "omega"; "simp" ])) "was stopped";
          Guide_client.close client;
          Fs.remove_tree dir);
    case "P12-05-01 a model file is read without running it" (fun () ->
        let model = is_ok "load" (Guide_model.load (Support.path "fixtures/guide/library/model.json")) in
        let record = Support.json "fixtures/guide/library/run.json" in
        equal_int "dim" (Json.int_or 0 "dim" (Option.get (Json.member "config" record))) model.Guide_model.dim;
        equal_int "max_length" 128 model.Guide_model.max_length;
        equal_string "pooling" "mean" model.Guide_model.pooling;
        equal_string "mixer" "none" model.Guide_model.mixer;
        equal_int "the tokens of the record of its run" (Json.int_or 0 "vocabulary" record) model.Guide_model.vocabulary;
        equal_int "the templates of the record" (Json.int_or 0 "classes" record) (List.length model.Guide_model.templates);
        equal_int "the weights of the record" (Json.int_or 0 "parameters" record) model.Guide_model.parameters;
        equal_string "the first class is that of all other tactics" "<other>" (List.hd model.Guide_model.templates).Guide_model.name;
        check "every closing rate is a share" (List.for_all (fun (c : Guide_model.template) -> c.Guide_model.closes >= 0.0 && c.Guide_model.closes <= 1.0) model.Guide_model.templates);
        let steps = List.fold_left (fun n (c : Guide_model.template) -> n + c.Guide_model.count) 0 model.Guide_model.templates in
        equal_int "the steps it was trained on" (Json.int_or 0 "train" (Option.get (Json.member "dataset" record))) steps;
        let text = Guide_model.render ~top:3 model in
        contains "what it is" text "tactic model  mean pooling, vectors of 32, reads up to 128 tokens";
        contains "its size" text (Printf.sprintf "%d weights, %d tokens, %d tactic templates from %d recorded steps" model.Guide_model.parameters model.Guide_model.vocabulary (List.length model.Guide_model.templates) steps);
        contains "the rest" text (Printf.sprintf "and %d more" (List.length model.Guide_model.templates - 3));
        check "no colour unless asked" (not (Text.contains text "\027["));
        check "colour when asked" (Text.contains (Guide_model.render ~color:true model) "\027[1mtactic model\027[0m");
        let summary = Guide_model.to_json model in
        check "the summary has no weights" (Json.member "weights" summary = None && Json.int_or 0 "parameters" summary = model.Guide_model.parameters);
        contains "a file that is not there" (is_error "absent" (Guide_model.load "/nowhere/model.json")) "there is no model at /nowhere/model.json";
        contains "another document" (is_error "run" (Guide_model.load (Support.path "fixtures/guide/library/run.json"))) "has schema \"maithic.guide.run/1\"; a model has \"maithic.guide.model/1\"";
        contains "no schema" (is_error "bare" (Guide_model.of_json (Json.Obj []))) "names no schema";
        contains "half a model" (is_error "half" (Guide_model.of_json (Json.Obj [ ("schema", Json.String Guide_model.schema) ]))) "has no member \"config\"") ]

let fallback_tests =
  [ case "P12-05-03 a guide that is down answers with equal priors and a value of zero" (fun () ->
        let advice = Guide_advice.unavailable "the Julia executable \"julia\" is not on the path" in
        close_list "equal priors" [ 0.25; 0.25; 0.25; 0.25 ] (Guide_advice.priors advice ~state:"s" [ "a"; "b"; "c"; "d" ]);
        close_list "one candidate" [ 1.0 ] (Guide_advice.priors advice ~state:"s" [ "a" ]);
        check "no candidates" (Guide_advice.priors advice ~state:"s" [] = []);
        close_to "value" 0.0 (Guide_advice.value advice ~state:"s");
        close_list "premise scores" [ 0.0; 0.0 ] (Guide_advice.premise_scores advice ~state:"s" [ "a"; "b" ]);
        check "the reason is kept" (Guide_advice.status advice = Guide_advice.Down "the Julia executable \"julia\" is not on the path");
        check "no model to speak of" (Guide_advice.info advice = None);
        let counts = Guide_advice.counts advice in
        equal_int "asked" 4 counts.Guide_advice.asked;
        equal_int "answered by the service" 0 counts.Guide_advice.answered;
        close_list "uniform" [ 0.5; 0.5 ] (Guide_advice.uniform 2);
        check "uniform over nothing" (Guide_advice.uniform 0 = []);
        check "as a document"
          (Json.equal (Guide_advice.to_json ~model:"m.json" advice)
             (Json.Obj
                [ ("model", Json.String "m.json"); ("status", Json.String "down"); ("reason", Json.String "the Julia executable \"julia\" is not on the path");
                  ("asked", Json.Int 4); ("answered", Json.Int 0) ]));
        equal_string "in a sentence"
          "the guide m.json was not used for 4 of 4 question(s), which were answered with equal priors: the Julia executable \"julia\" is not on the path"
          (Guide_advice.describe ~model:"m.json" advice));
    case "P12-05-03 a connection that fails gives a guide that is down, with the reason" (fun () ->
        let advice = Guide_advice.start (fun () -> Error "there is no model at m.json") in
        check "down" (Guide_advice.status advice = Guide_advice.Down "there is no model at m.json");
        close_list "equal priors" [ 0.5; 0.5 ] (Guide_advice.priors advice ~state:"s" [ "a"; "b" ]);
        let s = stand_in () in
        let up = Guide_advice.start (fun () -> Guide_client.connect (transport s)) in
        check "a connection that works gives a guide that is up" (Guide_advice.status up = Guide_advice.Up);
        check "with its model" (Option.map (fun (i : Guide_client.info) -> i.Guide_client.classes) (Guide_advice.info up) = Some 33));
    case "P12-05-03 the first failure takes the guide down for good, and the service is not asked again" (fun () ->
        let s = stand_in ~fail_from:3 () in
        let advice = Guide_advice.of_client (is_ok "connect" (Guide_client.connect (transport ~weight:(fun c -> if c = "b" then 3.0 else 1.0) s))) in
        close_list "the first answer is the service's" [ 0.25; 0.75 ] (Guide_advice.priors advice ~state:"one" [ "a"; "b" ]);
        close_to "so is the second" 0.25 (Guide_advice.value advice ~state:"two");
        check "up so far" (Guide_advice.status advice = Guide_advice.Up);
        close_list "the third fails, and is answered with equal priors" [ 0.5; 0.5 ] (Guide_advice.priors advice ~state:"three" [ "a"; "b" ]);
        check "down, with what went wrong" (Guide_advice.status advice = Guide_advice.Down "the pipe is broken");
        equal_int "the service was closed" 1 s.closed;
        let asked = List.length (questions s) in
        close_list "later questions get the same answer" [ 0.5; 0.5 ] (Guide_advice.priors advice ~state:"four" [ "a"; "b" ]);
        close_to "and a value of zero" 0.0 (Guide_advice.value advice ~state:"five");
        equal_int "without a word to the service" asked (List.length (questions s));
        let counts = Guide_advice.counts advice in
        equal_int "five questions" 5 counts.Guide_advice.asked;
        equal_int "two answered" 2 counts.Guide_advice.answered;
        equal_string "in a sentence" "the guide was not used for 3 of 5 question(s), which were answered with equal priors: the pipe is broken"
          (Guide_advice.describe advice));
    case "P12-05-03 an answer that cannot be trusted takes the guide down like a failure" (fun () ->
        let advice = Guide_advice.of_client (answering [ ("priors", floats [ 0.9; 0.9 ]); ("templates", texts [ "a"; "b" ]) ]) in
        close_list "equal priors instead" [ 0.5; 0.5 ] (Guide_advice.priors advice ~state:"s" [ "a"; "b" ]);
        (match Guide_advice.status advice with
         | Guide_advice.Down reason -> contains "why" reason "add up to 1.8, not to 1"
         | Guide_advice.Up -> fail "the guide should be down"));
    case "P12-05-03 a guide that answered to the end reports that it was up" (fun () ->
        let s = stand_in () in
        let advice = Guide_advice.of_client (is_ok "connect" (Guide_client.connect (transport s))) in
        ignore (Guide_advice.priors advice ~state:"s" [ "a"; "b" ]);
        ignore (Guide_advice.value advice ~state:"s");
        Guide_advice.close advice;
        equal_string "the service was told goodbye" "shutdown" (meth (List.hd s.seen));
        check "as a document"
          (Json.equal (Guide_advice.to_json ~model:"m.json" advice)
             (Json.Obj [ ("model", Json.String "m.json"); ("status", Json.String "up"); ("asked", Json.Int 2); ("answered", Json.Int 2) ]));
        equal_string "in a sentence" "the guide m.json answered 2 of 2 question(s)" (Guide_advice.describe ~model:"m.json" advice);
        (* After the end, a question is answered as when the guide is down. *)
        close_list "closed" [ 0.5; 0.5 ] (Guide_advice.priors advice ~state:"s" [ "a"; "b" ])) ]

(* A simulated prover: a goal is its target, and a rule says what a tactic leaves of a target. *)
module Sim = struct
  type t = { rules : ((string * string) * string list) list; states : (int, Bridge_types.goal list) Hashtbl.t; mutable next : int; mutable tried : string list list }

  let goal t target =
    t.next <- t.next + 1;
    { Bridge_types.goal_id = Printf.sprintf "g%d" t.next; tag = ""; hypotheses = []; target; pretty = "\xE2\x8A\xA2 " ^ target }

  let create rules target =
    let t = { rules; states = Hashtbl.create 16; next = 0; tried = [] } in
    Hashtbl.replace t.states 0 [ goal t target ];
    t

  let driver t =
    let run state index tactic =
      let before = Hashtbl.find t.states state in
      let focus = List.nth before index in
      match List.assoc_opt (focus.Bridge_types.target, tactic) t.rules with
      | None -> Bridge_types.Stopped { kind = Bridge_types.Tactic_failure; error = tactic ^ " failed"; diagnostics = [] }
      | Some targets ->
        let after = List.filteri (fun i _ -> i < index) before @ List.map (goal t) targets @ List.filteri (fun i _ -> i > index) before in
        t.next <- t.next + 1;
        Hashtbl.replace t.states t.next after;
        Bridge_types.Stepped { state = t.next; goals = after; solved = after = []; diagnostics = [] }
    in
    { Lean_search.try_tactics =
        (fun ~state ~goal tactics ->
          t.tried <- t.tried @ [ tactics ];
          Ok (List.map (run state goal) tactics));
      release = ignore }
end

(* A goal with two ways on. The long way looks better to the search and ends nowhere; the short
   way closes the goal in one more step. *)
let maze =
  [ (("start", "long way"), [ "a1" ]); (("a1", "on"), [ "a2" ]); (("a2", "on"), [ "a3" ]); (("start", "short way"), [ "b1" ]); (("b1", "done"), []) ]

let base : Lean_search.proposer = Lean_search.vocabulary [ ("long way", 0.9); ("on", 0.9); ("short way", 0.5); ("done", 0.9) ]

let search_with proposer =
  let sim = Sim.create maze "start" in
  match Lean_search.search ~proposer (Sim.driver sim) ~state:0 (Hashtbl.find sim.Sim.states 0) with
  | Ok found -> (found, sim)
  | Error e -> fail ("search: " ^ Bridge_error.to_string e)

let prior_tests =
  [ case "P12-05-02 the model's favourite keeps its prior and the others lose in proportion" (fun () ->
        let weighed = Guide_prior.reweigh [ ("a", 0.9); ("b", 0.8); ("c", 0.4) ] [ 0.1; 0.5; 0.4 ] in
        equal_strings "in the order of the search" [ "a"; "b"; "c" ] (List.map fst weighed);
        close_list "the priors" [ 0.18; 0.8; 0.32 ] (List.map snd weighed);
        (* A candidate the model rules out is still a candidate: its prior stays above zero. *)
        let sure = Guide_prior.reweigh [ ("x", 0.9); ("y", 0.1) ] [ 0.0; 1.0 ] in
        check "a model that is sure" (snd (List.hd sure) > 0.0 && snd (List.hd sure) < 1e-9);
        close_to "its favourite" 0.1 (snd (List.nth sure 1));
        check "every prior stays a prior" (List.for_all (fun (_, q) -> q > 0.0 && q <= 1.0) (weighed @ sure));
        check "nothing to weigh" (Guide_prior.reweigh [] [] = []));
    case "P12-05-02 equal priors change nothing, and neither does an answer that does not fit" (fun () ->
        let candidates = [ ("rfl", 0.9); ("induction n", 0.4); ("omega", 0.8); ("simp", 0.7) ] in
        check "equal priors" (Guide_prior.reweigh candidates (Guide_advice.uniform 4) = candidates);
        check "too few" (Guide_prior.reweigh candidates [ 0.5; 0.5 ] = candidates);
        check "all zero" (Guide_prior.reweigh candidates [ 0.0; 0.0; 0.0; 0.0 ] = candidates);
        check "not a number" (Guide_prior.reweigh candidates [ 0.5; Float.nan; 0.25; 0.25 ] = candidates);
        check "negative" (Guide_prior.reweigh candidates [ 0.5; -0.5; 0.5; 0.5 ] = candidates);
        check "without end" (Guide_prior.reweigh candidates [ 0.5; Float.infinity; 0.25; 0.25 ] = candidates));
    case "P12-05-02 the guide is asked about the goal as Lean prints it, with the candidates of the search" (fun () ->
        let s = stand_in () in
        let advice = Guide_advice.of_client (is_ok "connect" (Guide_client.connect (transport ~weight:(fun c -> if c = "omega" then 8.0 else 1.0) s))) in
        let pretty = "n : Nat\nh : 0 < n\n\xE2\x8A\xA2 n - 1 < n" in
        let goal =
          { Bridge_types.goal_id = "g"; tag = ""; target = "n - 1 < n"; pretty;
            hypotheses =
              [ { Bridge_types.hyp_name = "n"; hyp_type = "Nat"; hyp_value = None }; { Bridge_types.hyp_name = "h"; hyp_type = "0 < n"; hyp_value = None } ] }
        in
        let guided = Guide_prior.proposer advice ~base:(Lean_search.vocabulary [ ("rfl", 0.9); ("omega", 0.8); ("simp", 0.7) ]) goal in
        (match questions s with
         | [ request ] ->
           equal_string "the state" pretty (Json.string_or "" "state" request);
           equal_strings "the candidates, in the order of the search" [ "rfl"; "omega"; "simp" ] (candidates_of request)
         | _ -> fail "one question was expected");
        equal_strings "the candidates, as the search named them" [ "rfl"; "omega"; "simp" ] (List.map fst guided);
        close_list "the priors after the guide" [ 0.9 /. 8.0; 0.8; 0.7 /. 8.0 ] (List.map snd guided);
        equal_string "a goal Lean did not print is put together from its parts" pretty (Guide_prior.state_text { goal with Bridge_types.pretty = "" });
        equal_string "a definition among the hypotheses" "x : Nat := 2\n\xE2\x8A\xA2 x = 2"
          (Guide_prior.state_text
             { goal with Bridge_types.pretty = " "; target = "x = 2"; hypotheses = [ { Bridge_types.hyp_name = "x"; hyp_type = "Nat"; hyp_value = Some "2" } ] });
        (* A proposer with nothing to propose is not worth a question. *)
        check "no candidates" (Guide_prior.proposer advice ~base:(fun _ -> []) goal = []);
        equal_int "and no question" 1 (List.length (questions s)));
    case "P12-05-02 a guided search expands the goal the model points at first" (fun () ->
        let plain, _ = search_with base in
        check "without a guide the proof is found" (plain.Lean_search.stop = Search_kernel.Found);
        equal_string "the proof" "by\n  short way\n  done" (Option.get plain.Lean_search.proof);
        equal_int "after the long way was walked to its end" 5 plain.Lean_search.expansions;
        let s = stand_in () in
        let advice = Guide_advice.of_client (is_ok "connect" (Guide_client.connect (transport ~weight:(fun c -> if c = "short way" || c = "done" then 9.0 else 1.0) s))) in
        let guided, sim = search_with (Guide_prior.proposer advice ~base) in
        check "with the guide it is found too" (guided.Lean_search.stop = Search_kernel.Found);
        equal_string "the same proof" (Option.get plain.Lean_search.proof) (Option.get guided.Lean_search.proof);
        equal_int "in two expansions" 2 guided.Lean_search.expansions;
        equal_int "one question for each" 2 (List.length (questions s));
        equal_strings "the candidates go to Lean as the search named them" [ "long way"; "on"; "short way"; "done" ] (List.hd sim.Sim.tried));
    case "P12-05-03 a search with a guide that is down is the search without a guide" (fun () ->
        let plain, plain_sim = search_with base in
        let advice = Guide_advice.unavailable "the service could not be started" in
        let guided, guided_sim = search_with (Guide_prior.proposer advice ~base) in
        check "the same end" (guided.Lean_search.stop = plain.Lean_search.stop);
        check "the same proof" (guided.Lean_search.proof = plain.Lean_search.proof);
        equal_int "the same expansions" plain.Lean_search.expansions guided.Lean_search.expansions;
        check "the same tactics to Lean, in the same order" (guided_sim.Sim.tried = plain_sim.Sim.tried);
        equal_int "every goal was asked about" plain.Lean_search.expansions (Guide_advice.counts advice).Guide_advice.asked;
        (* A guide that fails in the middle of a search leaves the rest of the search as it was. *)
        let s = stand_in ~fail_from:2 () in
        let failing = Guide_advice.of_client (is_ok "connect" (Guide_client.connect (transport s))) in
        let half, _ = search_with (Guide_prior.proposer failing ~base) in
        check "found all the same" (half.Lean_search.proof = plain.Lean_search.proof);
        check "the guide went down on the way" (Guide_advice.status failing = Guide_advice.Down "the pipe is broken")) ]

let () = run "guide" [ ("client", client_tests); ("fallback", fallback_tests); ("prior", prior_tests) ]
