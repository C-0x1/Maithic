(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_gateway
open Maithic_ledger
open Maithic_prover
open Testkit

let root =
  let rec up dir depth =
    if Fs.exists Fs.(dir / "PROJECT.toml") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir || depth > 64 then failwith "PROJECT.toml not found" else up parent (depth + 1)
  in
  up (Sys.getcwd ()) 0

let path rel = Fs.(root / rel)
let read rel = match Fs.read (path rel) with Ok t -> t | Error e -> failwith e

let maithic =
  let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else path "ocaml/_build/default/bin/maithic/main.exe" in
  if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe

let located =
  match Bridge_locate.locate ~cwd:root () with
  | Ok l -> l
  | Error e ->
    prerr_endline ("integration tests need the Bridge: " ^ Bridge_error.to_string e ^ "\nBuild it with `make build-lean`.");
    exit 1

let bridge_ok what = function Ok v -> v | Error e -> fail (what ^ ": " ^ Bridge_error.to_string e)
let with_bridge f = bridge_ok "bridge" (Bridge_client.with_bridge located.Bridge_locate.argv (fun b -> Ok (f b)))

let session_tests =
  List.map
    (fun file ->
      let name = Filename.remove_extension file in
      case (Printf.sprintf "P09-04-08 the built Bridge answers the scripted session '%s' over its pipes" name) (fun () ->
          let session = Json.parse_exn (read ("fixtures/bridge/sessions/" ^ file)) in
          let worker = is_ok "spawn" (Proc.spawn (List.hd located.Bridge_locate.argv) []) in
          List.iteri
            (fun i step ->
              let line =
                match Json.member "raw" step, Json.member "request" step with
                | Some (Json.String raw), _ -> raw
                | _, Some rq -> Json.to_string rq
                | _ -> fail "step without a request"
              in
              ignore (is_ok "send" (Proc.send_line worker line));
              let answer = Json.parse_exn (is_ok "read" (Proc.read_line ~timeout:120.0 worker)) in
              match Json.is_subset ~expected:(Option.get (Json.member "expect" step)) answer with
              | Ok () -> ()
              | Error e -> fail (Printf.sprintf "step %d: %s" (i + 1) e))
            (Json.list_or_empty "steps" session);
          ignore (Proc.stop worker)))
    (List.filter (fun f -> Fs.extension f = ".json") (Fs.list_dir (path "fixtures/bridge/sessions")))

let even = read "examples/even.lean"
let triangle = read "examples/triangle.lean"
let recheck ~source name = Recheck.fresh ~argv:located.Bridge_locate.argv ~source name

let prove ?(policy = Trust_policy.default) ?gateway ?(with_recheck = true) ?(settings = Engine.default_settings) source name =
  with_bridge (fun bridge ->
      if with_recheck then Engine.prove ~recheck ?gateway ~bridge ~policy settings ~source name
      else Engine.prove ?gateway ~bridge ~policy settings ~source name)

let proved what = function Ok (r : Engine.result) -> r | Error f -> fail (what ^ ": " ^ Fault.to_string f)

let scripted answers =
  let handler, seen = Scripted.sequence (List.map (fun text -> Ok (Scripted.answer text)) answers) in
  let config = Maithic_config.Config.of_defaults () in
  match Gateway.create ~scripted:handler config with Ok g -> (g, seen) | Error f -> fail (Fault.to_string f)

let good_proof = "by\n  induction n with\n  | zero => rfl\n  | succ k ih => simp only [tri]; grind"

let engine_tests =
  [ case "P18-02-01 a proof found by the portfolio is re-checked in a fresh process before it is verified" (fun () ->
        let r = proved "prove" (prove even "even_add_even") in
        check "verified" (r.Engine.status = Status.Verified);
        equal_string "proof" "by omega" (Option.get r.Engine.proof);
        equal_string "proposer" "portfolio" (Option.get r.Engine.proposer);
        equal_string "statement as Lean prints it" "\xE2\x88\x80 (a b : Nat), a % 2 = 0 \xE2\x86\x92 b % 2 = 0 \xE2\x86\x92 (a + b) % 2 = 0" r.Engine.statement;
        equal_strings "axioms" [ "propext"; "Quot.sound" ] r.Engine.axioms;
        let outcome = Option.get r.Engine.recheck in
        check "re-check passed" outcome.Recheck.passed;
        equal_string "same statement" r.Engine.statement outcome.Recheck.statement;
        equal_string "lean" Version.lean_toolchain outcome.Recheck.lean;
        equal_int "no model round" 0 r.Engine.rounds_used);
    case "P18-03-02 without the re-check the same proof stays at checked" (fun () ->
        let r = proved "prove" (prove ~with_recheck:false even "even_add_even") in
        check "checked" (r.Engine.status = Status.Checked);
        check "the proof is still reported" (r.Engine.proof = Some "by omega");
        contains "reason" (String.concat " " r.Engine.decision.Trust_policy.reasons) "re-check");
    case "P18-02-02 the re-check sees only the file, never the state of the session" (fun () ->
        with_bridge (fun bridge ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header bridge []) in
            let _, kept = bridge_ok "define" (Bridge_client.run_command ~keep:true bridge ~env "def sessionOnly : Nat := 7") in
            let report, _ = bridge_ok "use" (Bridge_client.run_command bridge ~env:(Option.get kept) "theorem uses : sessionOnly = 7 := rfl") in
            check "the session accepts it" report.Bridge_types.ok;
            let outcome = bridge_ok "recheck" (recheck ~source:"theorem uses : sessionOnly = 7 := rfl\n" "uses") in
            check "the fresh process rejects it" (not outcome.Recheck.passed);
            contains "error" outcome.Recheck.first_error "sessionOnly"));
    case "P01-03-02 a failed model round is followed by a repair round that carries Lean's errors" (fun () ->
        let gateway, seen =
          scripted [ {|{"candidates": ["by omega", "by simp [tri]"]}|}; Printf.sprintf {|{"candidates": [%s]}|} (Json.to_string (Json.String good_proof)) ]
        in
        let r = proved "prove" (prove ~gateway triangle "tri_formula") in
        check "verified" (r.Engine.status = Status.Verified);
        equal_string "proposer" "model" (Option.get r.Engine.proposer);
        equal_int "rounds" 2 r.Engine.rounds_used;
        (match seen () with
         | [ first; second ] ->
           equal_string "first prompt" "prove.propose" first.Gateway_types.purpose;
           equal_string "second prompt" "prove.repair" second.Gateway_types.purpose;
           let user = (List.hd second.Gateway_types.messages).Gateway_types.content in
           contains "the failed proof is shown" user "by simp [tri]";
           contains "with Lean's goal" user "2 * tri n = n * (n + 1)";
           contains "and a hint" user "hint:";
           check "portfolio failures are not repeated to the model" (not (Text.contains user "by trivial"));
           contains "the theorem is passed as data" (List.hd first.Gateway_types.messages).Gateway_types.content "<data>\ntheorem tri_formula (n : Nat) : 2 * tri n = n * (n + 1)";
           contains "the context includes the definition" (List.hd first.Gateway_types.messages).Gateway_types.content "def tri : Nat";
           check "a schema is requested" (first.Gateway_types.schema <> None)
         | other -> fail (Printf.sprintf "expected two model calls, saw %d" (List.length other))));
    case "P01-03-03 a proof the model repeats is not sent to Lean again" (fun () ->
        let gateway, _ =
          scripted
            [ {|{"candidates": ["by omega", "by  simp [tri]"]}|};
              Printf.sprintf {|{"candidates": ["by omega", "by simp   [tri]", %s]}|} (Json.to_string (Json.String good_proof)) ]
        in
        let r = proved "prove" (prove ~gateway triangle "tri_formula") in
        check "verified" (r.Engine.status = Status.Verified);
        let model_attempts = List.filter (fun (a : Attempts.attempt) -> a.Attempts.round >= 1) r.Engine.attempts in
        equal_strings "each distinct proof once" [ "by simp [tri]"; Attempts.normalize good_proof ]
          (List.map (fun (a : Attempts.attempt) -> Attempts.normalize a.Attempts.proof) model_attempts));
    case "P09-05-05 forbidden constructs in a model answer never reach Lean and are never verified" (fun () ->
        let gateway, _ =
          scripted
            [ {|{"candidates": ["by sorry", "by native_decide", "by exact (by run_tac (pure ()))", "rfl\naxiom cheat : False", "by exact absurd rfl (IO.getStdout)"]}|};
              {|{"candidates": []}|} ]
        in
        let r = proved "prove" (prove ~gateway ~settings:{ Engine.default_settings with Engine.candidates = 5 } triangle "tri_formula") in
        check "failed" (r.Engine.status = Status.Failed);
        let model_attempts = List.filter (fun (a : Attempts.attempt) -> a.Attempts.round >= 1) r.Engine.attempts in
        equal_int "five candidates" 5 (List.length model_attempts);
        check "all stopped by the screen" (List.for_all (fun (a : Attempts.attempt) -> a.Attempts.error_class = Some Attempts.Guard_rejected) model_attempts);
        contains "note" r.Engine.ai_note "nothing new");
    case "P18-03-01 a proof that needs an axiom outside the policy is not accepted" (fun () ->
        let source = "theorem lem (p : Prop) : p \xE2\x88\xA8 \xC2\xACp := by\n  sorry\n" in
        let classical = proved "default policy" (prove source "lem") in
        check "verified with choice allowed" (classical.Engine.status = Status.Verified && List.mem "Classical.choice" classical.Engine.axioms);
        let constructive = { Trust_policy.default with Trust_policy.name = "constructive"; allowed_axioms = [ "propext"; "Quot.sound" ] } in
        let strict = proved "constructive policy" (prove ~policy:constructive ~settings:{ Engine.default_settings with Engine.use_ai = false } source "lem") in
        check "failed" (strict.Engine.status = Status.Failed);
        check "the reason names the axiom"
          (List.exists (fun (a : Attempts.attempt) -> Text.contains a.Attempts.error "axioms outside the policy: Classical.choice") strict.Engine.attempts));
    case "the engine reports what it cannot start on with a stable code" (fun () ->
        (match prove even "no_such_theorem" with
         | Error f -> equal_string "code" "MTH-PROVE-0001" f.Fault.code; check "usage" (f.Fault.exit = Exit_code.Usage)
         | Ok _ -> fail "expected a fault");
        match prove "def broken : Nat := \"text\"\n\ntheorem t : True := by\n  sorry\n" "t" with
        | Error f -> equal_string "code" "MTH-PROVE-0002" f.Fault.code; contains "Lean's error is shown" f.Fault.message "Type mismatch"
        | Ok _ -> fail "expected a fault");
    case "the model is not asked when it is unavailable, and the result says why" (fun () ->
        let config = Maithic_config.Config.of_defaults () in
        let gateway = match Gateway.create ~getenv:(fun _ -> None) config with Ok g -> g | Error f -> fail (Fault.to_string f) in
        let r = proved "prove" (prove ~gateway triangle "tri_formula") in
        check "failed" (r.Engine.status = Status.Failed);
        contains "note" r.Engine.ai_note "no API key for anthropic (set ANTHROPIC_API_KEY)") ]

let bridge_tests =
  [ case "P09-04-03 the Bridge runs through lake env inside a project and imports its modules" (fun () ->
        let in_project = bridge_ok "locate" (Bridge_locate.locate ~project:(path "lean") ~cwd:root ()) in
        equal_strings "command" [ "lake"; "env" ] (List.filteri (fun i _ -> i < 2) in_project.Bridge_locate.argv);
        let report =
          bridge_ok "bridge"
            (Bridge_client.with_bridge ?cwd:in_project.Bridge_locate.cwd in_project.Bridge_locate.argv (fun b ->
                 Result.map fst
                   (Bridge_client.check_file b "import MaithicBench\n\ntheorem solved : MaithicBench.Basics.add_comm' := MaithicBench.Basics.Solutions.add_comm'\n")))
        in
        check "ok" report.Bridge_types.ok;
        equal_string "statement" "MaithicBench.Basics.add_comm'" (List.hd report.Bridge_types.decls).Bridge_types.type_;
        (* The modules of a project are found only through the project: with a project of its own, a file
           imports what that project built, and outside it the same import is an error, reported as such. *)
        let other = Fs.temp_dir "maithic-project" in
        ignore (is_ok "lakefile" (Fs.write Fs.(other / "lakefile.toml") "name = \"other\"\ndefaultTargets = [\"Other\"]\n\n[[lean_lib]]\nname = \"Other\"\n"));
        ignore (is_ok "toolchain" (Fs.write Fs.(other / "lean-toolchain") (read "lean-toolchain")));
        ignore (is_ok "module" (Fs.write Fs.(other / "Other.lean") "def Other.answer : Nat := 42\n"));
        let built = is_ok "lake build" (Proc.run ~cwd:other ~timeout:600.0 "lake" [ "build" ]) in
        equal_int "the other project builds" 0 built.Proc.status;
        let elsewhere = bridge_ok "locate" (Bridge_locate.locate ~project:other ~cwd:root ()) in
        let uses_other = "import Other\n\ntheorem answered : Other.answer = 42 := rfl\n" in
        let inside =
          bridge_ok "bridge" (Bridge_client.with_bridge ?cwd:elsewhere.Bridge_locate.cwd elsewhere.Bridge_locate.argv (fun b -> Result.map fst (Bridge_client.check_file b uses_other)))
        in
        check "inside its project" inside.Bridge_types.ok;
        (match with_bridge (fun b -> Bridge_client.check_file b uses_other) with
         | Error (Bridge_error.Remote { tag; _ }) -> equal_string "tag" "MTH-BRG-0101" tag
         | Error e -> fail (Bridge_error.to_string e)
         | Ok _ -> fail "expected an import error");
        (* The libraries the Bridge was built with are found from anywhere, the other project included:
           this is how a file of any project can use the statement tests. *)
        let tested = "import Maithic.Refute.Testing.Command\n\n#maithic_test \xE2\x88\x80 n : Nat, n + 0 = n\n" in
        let anywhere = bridge_ok "own library" (with_bridge (fun b -> Result.map fst (Bridge_client.check_file b tested))) in
        check "with no project" anywhere.Bridge_types.ok;
        let from_other =
          bridge_ok "bridge" (Bridge_client.with_bridge ?cwd:elsewhere.Bridge_locate.cwd elsewhere.Bridge_locate.argv (fun b -> Result.map fst (Bridge_client.check_file b tested)))
        in
        check "and from another project" from_other.Bridge_types.ok;
        contains "where the test ran" (String.concat "\n" (List.map (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.text) from_other.Bridge_types.diagnostics)) "passed 100 cases";
        Fs.remove_tree other);
    case "P09-05-04 heartbeat and recursion limits reach the elaborator" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let source = "theorem slow (xs ys zs : List Nat) : (xs ++ ys ++ zs).reverse.length = zs.length + (ys.length + xs.length) := by grind" in
            let limited, _ = bridge_ok "limited" (Bridge_client.run_command ~limits:{ Bridge_types.no_limits with Bridge_types.max_heartbeats = 1 } b ~env source) in
            check "fails under one thousand heartbeats" (not limited.Bridge_types.ok);
            contains "reason" (Bridge_types.error_text limited) "maximum number of heartbeats";
            check "classified as a timeout" (Attempts.classify (Bridge_types.error_text limited) = Attempts.Timeout);
            let free, _ = bridge_ok "default" (Bridge_client.run_command b ~env source) in
            check "passes with the default budget" free.Bridge_types.ok));
    case "P09-01-06 a proof found tactic by tactic is verified again as a whole declaration" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let statement = "theorem pair (p q : Prop) (hp : p) (hq : q) : p \xE2\x88\xA7 q" in
            let report, opened = bridge_ok "start" (Bridge_client.start_proof b ~env (`Statement statement)) in
            check "the statement elaborates" report.Bridge_types.ok;
            let root = match opened with [ o ] -> o.Bridge_types.state | _ -> fail "expected one proof state" in
            (* A breadth-first search over a small vocabulary. Every state is kept by the Bridge,
               so each expansion starts from where its parent left off. *)
            let vocabulary = [ "exact hp"; "exact hq"; "constructor"; "rfl" ] in
            let rec search frontier depth =
              if depth > 3 || frontier = [] then None
              else begin
                let expand (state, script) =
                  let outcomes = bridge_ok "expand" (Bridge_client.try_tactics b ~state vocabulary) in
                  List.concat
                    (List.map2
                       (fun tactic outcome ->
                         match outcome with
                         | Bridge_types.Stepped { state; solved; _ } -> [ (state, script @ [ tactic ], solved) ]
                         | Bridge_types.Stopped _ -> [])
                       vocabulary outcomes)
                in
                let children = List.concat_map expand frontier in
                match List.find_opt (fun (_, _, solved) -> solved) children with
                | Some (_, script, _) -> Some script
                | None -> search (List.map (fun (state, script, _) -> (state, script)) children) (depth + 1)
              end
            in
            let script = match search [ (root, []) ] 1 with Some s -> s | None -> fail "no proof within three steps" in
            equal_strings "the shortest script in this vocabulary" [ "constructor"; "exact hp"; "exact hq" ] script;
            let unchanged = bridge_ok "root" (Bridge_client.get_state b root) in
            check "the root state is as it was" ((not unchanged.Bridge_types.solved) && List.length unchanged.Bridge_types.goals = 1);
            (* A closed proof state is evidence, not a verdict: the script is checked as a declaration. *)
            let source = statement ^ " := by\n  " ^ String.concat "\n  " script in
            let verified, _ = bridge_ok "verify" (Bridge_client.run_command ~names:[ "pair" ] b ~env source) in
            check "the kernel accepts the finished declaration" verified.Bridge_types.ok;
            let decl = List.hd verified.Bridge_types.decls in
            check "it is a theorem that rests on no sorry" (decl.Bridge_types.kind = "theorem" && not decl.Bridge_types.uses_sorry);
            let facts = bridge_ok "stats" (Bridge_client.stats b) in
            check "the Bridge counted the tactic runs" (Json.int_or 0 "tacticRuns" facts >= 3 * List.length vocabulary)));
    case "the server reports itself healthy and compatible" (fun () ->
        with_bridge (fun b ->
            let h = Bridge_client.handshake b in
            equal_string "version" Version.version h.Bridge_types.bridge;
            equal_string "lean" Version.lean_toolchain h.Bridge_types.lean;
            let healthy, checks = bridge_ok "self-test" (Bridge_client.self_test b) in
            check "healthy" healthy;
            equal_int "checks" 5 (List.length checks))) ]

(* ---- the step-wise search, on the real Bridge ---- *)

let double = read "examples/double.lean"

let search_tests =
  let offline = { Engine.default_settings with Engine.use_ai = false; search = true } in
  [ case "P01-01-07 the search finds by induction what the portfolio does not, and the proof goes through the trust path" (fun () ->
        let without = proved "prove" (prove ~settings:{ offline with Engine.search = false } double "double_eq") in
        check "the portfolio alone fails" (without.Engine.status = Status.Failed);
        let r = proved "prove" (prove ~settings:offline double "double_eq") in
        check "verified" (r.Engine.status = Status.Verified);
        equal_string "proposer" Engine.search_name (Option.get r.Engine.proposer);
        equal_string "proof" "by\n  induction n\n  \xC2\xB7 rfl\n  \xC2\xB7 simp [double]\n    omega" (Option.get r.Engine.proof);
        check "the fresh process accepted the same text" (Option.get r.Engine.recheck).Recheck.passed;
        equal_int "no model round" 0 r.Engine.rounds_used;
        let last = List.nth r.Engine.attempts (List.length r.Engine.attempts - 1) in
        check "the script is recorded as an attempt of the search" (last.Attempts.accepted && last.Attempts.proposer = Engine.search_name);
        check "the portfolio was tried first" (List.exists (fun (a : Attempts.attempt) -> a.Attempts.proposer = Portfolio.name && not a.Attempts.accepted) r.Engine.attempts));
    case "P01-01-07 the theorem of the model demonstration is proved by search with no model" (fun () ->
        let r = proved "prove" (prove ~settings:offline triangle "tri_formula") in
        check "verified" (r.Engine.status = Status.Verified);
        equal_string "proposer" Engine.search_name (Option.get r.Engine.proposer);
        contains "by induction" (Option.get r.Engine.proof) "induction n");
    case "P01-01-06 on the Bridge, the goals a tactic leaves are searched one by one" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let proposer = Lean_search.vocabulary [ ("constructor", 0.5); ("exact hp", 0.5); ("exact hq", 0.5) ] in
            let found =
              bridge_ok "search"
                (Lean_search.prove_statement ~proposer b ~env "theorem pair (p q : Prop) (hp : p) (hq : q) : p \xE2\x88\xA7 q")
            in
            check "found" (found.Lean_search.stop = Search_kernel.Found);
            equal_string "proof" "by\n  constructor\n  \xC2\xB7 exact hp\n  \xC2\xB7 exact hq" (Option.get found.Lean_search.proof);
            equal_int "three goals expanded" 3 found.Lean_search.expansions;
            let facts = bridge_ok "stats" (Bridge_client.stats b) in
            equal_int "every proof state was released" 0 (Json.int_or (-1) "proofStates" facts)));
    case "P01-03-05 on the Bridge, a tactic that changes nothing is not a step" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let only_skip = bridge_ok "search" (Lean_search.prove_statement ~proposer:(Lean_search.vocabulary [ ("skip", 0.9) ]) b ~env "theorem four : 2 + 2 = 4") in
            check "skip proves nothing" (only_skip.Lean_search.proof = None && only_skip.Lean_search.stop = Search_kernel.Exhausted);
            let with_rfl =
              bridge_ok "search" (Lean_search.prove_statement ~proposer:(Lean_search.vocabulary [ ("skip", 0.9); ("rfl", 0.1) ]) b ~env "theorem four : 2 + 2 = 4")
            in
            equal_string "proof" "by\n  rfl" (Option.get with_rfl.Lean_search.proof)));
    case "P01-01-05 a search for a false statement stops at its budget and proposes nothing" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let budget = { Search_kernel.unlimited with Search_kernel.expansions = 4 } in
            let found = bridge_ok "search" (Lean_search.prove_statement ~budget b ~env "theorem wrong (n : Nat) : n + 1 = n") in
            check "no proof" (found.Lean_search.proof = None);
            check "within the budget" (found.Lean_search.expansions <= 4);
            let unreadable = bridge_ok "search" (Lean_search.prove_statement b ~env "theorem broken (n : Nat) : n +") in
            check "a statement that does not elaborate is not searched" (unreadable.Lean_search.proof = None && unreadable.Lean_search.expansions = 0)));
    case "P01-01-07 prove --search reports the proof and the proposer, in text and in JSON" (fun () ->
        let env = [ ("MAITHIC_GATEWAY_MODE", "off"); ("MAITHIC_HOME", Fs.(Filename.get_temp_dir_name () / "maithic-no-home")); ("NO_COLOR", "1") ] in
        let run args = is_ok "run" (Proc.run ~cwd:root ~env ~timeout:300.0 maithic args) in
        let plain = run [ "prove"; "examples/double.lean"; "--no-ai" ] in
        equal_int "without the search the theorem is not proved" (Exit_code.to_int Exit_code.Not_verified) plain.Proc.status;
        let text = run [ "prove"; "examples/double.lean"; "--no-ai"; "--search" ] in
        equal_int "exit" 0 text.Proc.status;
        contains "status and proposer" text.Proc.stdout "double_eq  verified  (search,";
        contains "the script" text.Proc.stdout "induction n";
        let json = run [ "prove"; "examples/double.lean"; "--no-ai"; "--search"; "--json" ] in
        let result = List.hd (Json.list_or_empty "results" (Json.parse_exn json.Proc.stdout)) in
        equal_string "proposer" "search" (Json.string_or "" "proposer" result);
        let schema = is_ok "schema" (Json_schema.load (path "schemas/prove/run.schema.json")) in
        (match Json_schema.conforms schema (Json.parse_exn json.Proc.stdout) with Ok () -> () | Error e -> fail e)) ]

(* ---- the oracles: the command, the Julia engines and the Lean checker together ---- *)

let need_julia () = if Proc.find_in_path "julia" = None then skip "Julia is not installed; the oracle engines need it"

let oracle_tests =
  let env = [ ("MAITHIC_GATEWAY_MODE", "off"); ("MAITHIC_HOME", Fs.(Filename.get_temp_dir_name () / "maithic-no-home")); ("NO_COLOR", "1") ] in
  let run args = is_ok "run" (Proc.run ~cwd:root ~env ~timeout:300.0 maithic args) in
  let gap =
    "import Maithic.Oracle.Logic.LinearIntTactic\n\n\
     theorem no_point (x y : Int) (h\xE2\x82\x81 : 2 * x + 4 * y < 7) (h\xE2\x82\x82 : x + 2 * y \xE2\x89\xA5 4) (hp : x = x \xE2\x88\xA8 True) : False := by\n  sorry\n\n\
     theorem wrong (x y : Int) (h : x \xE2\x89\xA4 y) : x + 1 \xE2\x89\xA4 y := by\n  sorry\n"
  in
  [ case "P13-01-03 oracle list names the oracles and the goals they take" (fun () ->
        need_julia ();
        let out = run [ "oracle"; "list"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        equal_strings "names" [ "linear"; "polynomial"; "real" ]
          (List.map (Json.string_or "" "name") (Json.list_or_empty "oracles" (Json.parse_exn out.Proc.stdout)));
        contains "the table" (run [ "oracle"; "list" ]).Proc.stdout "Linear arithmetic by exact simplex");
    case "P13-01-05 oracle solve sends a goal document to the Julia engine and records the answer" (fun () ->
        need_julia ();
        let out = run [ "oracle"; "solve"; "fixtures/oracle/goals/linear-integer-gap.json"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let record = Json.parse_exn out.Proc.stdout in
        (match Json_schema.conforms (is_ok "schema" (Json_schema.load (path "schemas/oracle/result.schema.json"))) record with
         | Ok () -> ()
         | Error e -> fail e);
        equal_string "oracle" "linear" (Json.string_or "" "oracle" record);
        equal_string "outcome" "proved" (Json.string_or "" "outcome" record);
        equal_string "kind" "linear.farkas" (Json.string_or "" "kind" record);
        (* The engine is the same code that wrote the fixture, so the certificate is the same one. *)
        let fixture = Option.get (Json.member "certificate" (Json.parse_exn (read "fixtures/oracle/outcomes/linear-integer-gap.json"))) in
        equal_string "the certificate of the fixture" (Maithic_oracle.Oracle_outcome.certificate_id fixture) (Json.string_or "" "certificate_id" record));
    case "P13-01-05 a refuted goal is answered with the point where it fails, and its own exit status" (fun () ->
        need_julia ();
        let out = run [ "oracle"; "solve"; "fixtures/oracle/goals/linear-refuted.json" ] in
        equal_int "exit" (Exit_code.to_int Exit_code.Refuted) out.Proc.status;
        contains "the point" out.Proc.stdout "fails at:     x = 3, y = 0";
        let unknown = run [ "oracle"; "solve"; "fixtures/oracle/goals/linear-unknown.json" ] in
        equal_int "unknown is not verified" (Exit_code.to_int Exit_code.Not_verified) unknown.Proc.status;
        contains "the reason" unknown.Proc.stdout "reason:";
        let wrong = run [ "oracle"; "solve"; "fixtures/oracle/goals/linear-refuted.json"; "--oracle"; "sat" ] in
        equal_int "an oracle that does not exist" (Exit_code.to_int Exit_code.Usage) wrong.Proc.status;
        contains "the names" wrong.Proc.stderr "the oracles are linear, polynomial, real");
    case "P13-01-06 the certificate of a theorem's goal, handed to its Lean checker, proves the theorem" (fun () ->
        need_julia ();
        let dir = temp_dir "oracle" in
        let file = Fs.(dir / "Gap.lean") in
        ignore (is_ok "write" (Fs.write file gap));
        let out = run [ "oracle"; "solve"; file; "--theorem"; "no_point"; "--project"; "lean"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let record = Json.parse_exn out.Proc.stdout in
        (* The goal was exported from Lean with its own names; what is not arithmetic was left out. *)
        equal_strings "hypotheses sent" [ "h\xE2\x82\x81"; "h\xE2\x82\x82" ]
          (List.map (Json.string_or "" "name") (Json.list_or_empty "hypotheses" (Option.get (Json.member "goal" record))));
        let script = match Json.list_or_empty "scripts" record with Json.String s :: _ -> s | _ -> fail "a script for Lean" in
        equal_string "the script replays the certificate with Lean's names"
          "maithic_linear_int [x, y] [(1, \xE2\x9F\xA8[1, 2], .le, 3\xE2\x9F\xA9), (1, \xE2\x9F\xA8[-1, -2], .le, -4\xE2\x9F\xA9)] using [h\xE2\x82\x81, h\xE2\x82\x82]"
          script;
        (* Put where the sorry was, the script is checked by Lean like any other proof. *)
        let decl = is_ok "find" (Lean_source.find gap "no_point") in
        let proved = Fs.(dir / "Proved.lean") in
        ignore (is_ok "write" (Fs.write proved (decl.Lean_source.prelude ^ Lean_source.assemble decl (Lean_search.script_to_proof [ script ]))));
        let checked = run [ "check"; proved; "--project"; "lean" ] in
        equal_int "the kernel accepts it" 0 checked.Proc.status;
        contains "verified" checked.Proc.stdout "no_point  verified");
    case "P13-01-04 a theorem that is false is refuted from its exported goal" (fun () ->
        need_julia ();
        let dir = temp_dir "oracle" in
        let file = Fs.(dir / "Gap.lean") in
        ignore (is_ok "write" (Fs.write file gap));
        let out = run [ "oracle"; "solve"; file; "--theorem"; "wrong"; "--project"; "lean" ] in
        equal_int "exit" (Exit_code.to_int Exit_code.Refuted) out.Proc.status;
        contains "the point" out.Proc.stdout "fails at:     x = 0, y = 0";
        let missing = run [ "oracle"; "solve"; file; "--theorem"; "absent"; "--project"; "lean" ] in
        equal_int "a theorem that is not there" (Exit_code.to_int Exit_code.Usage) missing.Proc.status);
    case "P13-01-06 what an advisor proposes goes through the same checks as any proof" (fun () ->
        let settings = { Engine.default_settings with Engine.use_ai = false } in
        let asked = ref 0 in
        let advisor : Engine.advisor = ("oracle", fun ~env:_ _ -> incr asked; [ "by constructor"; "by simp"; good_proof ]) in
        let r = proved "prove" (with_bridge (fun bridge -> Engine.prove ~recheck ~advisors:[ advisor ] ~bridge ~policy:Trust_policy.default settings ~source:triangle "tri_formula")) in
        check "verified" (r.Engine.status = Status.Verified);
        equal_string "recorded under the name of the advisor" "oracle" (Option.get r.Engine.proposer);
        equal_int "asked once, after the portfolio failed" 1 !asked;
        check "its first proposal failed and was recorded"
          (List.exists (fun (a : Attempts.attempt) -> a.Attempts.proposer = "oracle" && a.Attempts.proof = "by constructor" && not a.Attempts.accepted) r.Engine.attempts);
        equal_int "a proposal the portfolio had tried already is not sent again" 2
          (List.length (List.filter (fun (a : Attempts.attempt) -> a.Attempts.proposer = "oracle") r.Engine.attempts));
        (* A theorem the portfolio proves never reaches the advisor. *)
        let quiet = ref 0 in
        let unused : Engine.advisor = ("oracle", fun ~env:_ _ -> incr quiet; []) in
        let easy = proved "prove" (with_bridge (fun bridge -> Engine.prove ~recheck ~advisors:[ unused ] ~bridge ~policy:Trust_policy.default settings ~source:even "even_add_even")) in
        check "verified by the portfolio" (easy.Engine.proposer = Some Portfolio.name && !quiet = 0));
    case "P13-01-06 prove --oracle asks the oracles and still reports a kernel-checked result" (fun () ->
        need_julia ();
        let dir = temp_dir "oracle" in
        let file = Fs.(dir / "Gap.lean") in
        ignore (is_ok "write" (Fs.write file gap));
        let out = run [ "prove"; file; "--theorem"; "no_point"; "--no-ai"; "--oracle"; "--project"; "lean" ] in
        equal_int "exit" 0 out.Proc.status;
        contains "verified" out.Proc.stdout "no_point  verified";
        (* The false theorem stays unproved: an oracle's refutation is not a proof, and no proof exists. *)
        let wrong = run [ "prove"; file; "--theorem"; "wrong"; "--no-ai"; "--oracle"; "--project"; "lean" ] in
        equal_int "not verified" (Exit_code.to_int Exit_code.Not_verified) wrong.Proc.status) ]

(* ---- the command, as a user runs it ---- *)

let clean_env = [ ("MAITHIC_GATEWAY_MODE", "replay"); ("MAITHIC_HOME", Fs.(Filename.get_temp_dir_name () / "maithic-no-home")); ("NO_COLOR", "1") ]

let run_cli ?(env = clean_env) ?stdin args =
  (* No key of the person running the tests may reach the command. *)
  let blank = List.filter (fun (k, _) -> not (List.mem_assoc k env)) [ ("ANTHROPIC_API_KEY", ""); ("OPENAI_API_KEY", ""); ("XAI_API_KEY", "") ] in
  is_ok "run" (Proc.run ?stdin ~cwd:root ~env:(blank @ env) ~timeout:300.0 maithic args)

let json_of (out : Proc.output) = match Json.parse out.Proc.stdout with Ok j -> j | Error e -> fail (e ^ "\n" ^ out.Proc.stdout ^ out.Proc.stderr)

let cli_tests =
  [ case "P00-01-02 version prints the version and the versions of its components" (fun () ->
        let out = run_cli [ "version" ] in
        equal_int "exit" 0 out.Proc.status;
        contains "version" out.Proc.stdout ("maithic " ^ Version.version);
        contains "lean" out.Proc.stdout Version.lean_toolchain;
        let j = json_of (run_cli [ "version"; "--json" ]) in
        equal_string "json" Version.version (Json.string_or "" "maithic" j);
        equal_string "protocol" (string_of_int Version.bridge_protocol) (Json.string_or "" "bridge_protocol" j));
    case "P00-01-03 doctor checks toolchains, the Bridge, the provider and keys" (fun () ->
        let out = run_cli [ "doctor"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let j = json_of out in
        check "healthy" (Json.member "healthy" j = Some (Json.Bool true));
        let components = Json.list_or_empty "components" j in
        equal_strings "components" [ "lean"; "lake"; "maithic-bridge"; "curl"; "julia"; "Rscript" ] (List.map (Json.string_or "" "name") components);
        let bridge = List.find (fun c -> Json.string_or "" "name" c = "maithic-bridge") components in
        contains "self-test" (Json.string_or "" "detail" bridge) "self-test healthy";
        equal_string "mode" "replay" (Option.get (Option.bind (Json.path [ "ai"; "gateway mode" ] j) Json.string_opt));
        (* A Bridge that cannot be found is a missing required component, with a remedy. *)
        let missing = run_cli ~env:(clean_env @ [ ("MAITHIC_BRIDGE", "/nonexistent/maithic-bridge") ]) [ "doctor" ] in
        equal_int "exit" 4 missing.Proc.status;
        contains "remedy" missing.Proc.stdout "missing");
    case "P09-05-03 doctor reports which isolation features are active" (fun () ->
        let j = json_of (run_cli [ "doctor"; "--json" ]) in
        contains "scrubbing" (Option.get (Option.bind (Json.path [ "isolation"; "environment scrubbing" ] j) Json.string_opt)) "active";
        contains "screening" (Option.get (Option.bind (Json.path [ "isolation"; "candidate screening" ] j) Json.string_opt)) "active");
    case "P00-01-05 prove --json emits the result manifest and exits with 0 for verified" (fun () ->
        let out = run_cli [ "prove"; "examples/even.lean"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let j = json_of out in
        equal_string "schema" "maithic.prove.run/1" (Json.string_or "" "schema" j);
        let result = List.hd (Json.list_or_empty "results" j) in
        equal_string "status" "verified" (Json.string_or "" "status" result);
        equal_string "proof" "by omega" (Json.string_or "" "proof" result);
        equal_string "schema" "maithic.prove.result/1" (Json.string_or "" "schema" result);
        check "re-check recorded" (Json.path [ "recheck"; "passed" ] result = Some (Json.Bool true));
        check "no model call was needed" (Json.path [ "gateway"; "calls" ] j = Some (Json.Int 0));
        equal ~show:Json.to_string "summary" (Json.Int 1) (Option.get (Json.path [ "summary"; "verified" ] j)));
    case "P10-07-04 the model-assisted proof replays from cassettes for each provider, with no key" (fun () ->
        List.iter
          (fun provider ->
            let out = run_cli [ "prove"; "examples/triangle.lean"; "--provider"; provider; "--json" ] in
            equal_int (provider ^ " exit") 0 out.Proc.status;
            let j = json_of out in
            let result = List.hd (Json.list_or_empty "results" j) in
            equal_string "status" "verified" (Json.string_or "" "status" result);
            equal_string "proposer" "model" (Json.string_or "" "proposer" result);
            equal_int "rounds" 2 (Json.int_or 0 "rounds_used" result);
            equal ~show:Json.to_string "calls" (Json.Int 2) (Option.get (Json.path [ "gateway"; "calls" ] j));
            equal ~show:Json.to_string "mode" (Json.String "replay") (Option.get (Json.path [ "gateway"; "mode" ] j)))
          [ "anthropic"; "openai"; "xai" ]);
    case "P10-07-05 a replay without its cassette fails with the documented code and exit status" (fun () ->
        let out = run_cli [ "prove"; "examples/triangle.lean"; "--tier"; "fast"; "--json" ] in
        equal_int "not verified" 7 out.Proc.status;
        let result = List.hd (Json.list_or_empty "results" (json_of out)) in
        equal_string "status" "failed" (Json.string_or "" "status" result);
        contains "note" (Json.string_or "" "ai_note" result) "no cassette for request");
    case "P00-01-01 global options override the configuration" (fun () ->
        let j = json_of (run_cli [ "config"; "show"; "--provider"; "xai"; "--model"; "grok-x"; "--mode"; "mock"; "--json" ]) in
        let value key = Option.get (Option.bind (Json.path [ key; "value" ] j) Json.string_opt) in
        equal_strings "values" [ "xai"; "grok-x"; "mock" ] [ value "ai.provider"; value "ai.model"; value "gateway.mode" ];
        equal_string "source" "command line" (Option.get (Option.bind (Json.path [ "ai.provider"; "source" ] j) Json.string_opt)));
    case "P00-01-06 a usage or configuration mistake is explained and exits with its code" (fun () ->
        let missing = run_cli [ "prove"; "no/such/file.lean" ] in
        equal_int "usage" 2 missing.Proc.status;
        contains "code" missing.Proc.stderr "MTH-CLI-0002";
        let bad = run_cli ~env:(clean_env @ [ ("MAITHIC_TIER", "fastest") ]) [ "config"; "validate" ] in
        equal_int "configuration" 3 bad.Proc.status;
        contains "variable" bad.Proc.stderr "MAITHIC_TIER";
        let unknown = run_cli [ "prove"; "examples/even.lean"; "--no-such-option" ] in
        equal_int "option" 2 unknown.Proc.status;
        contains "the option" unknown.Proc.stderr "maithic prove: unknown option --no-such-option";
        (* With --json a mistake in the arguments is a document, like every other error. *)
        let error_of out = Option.get (Json.member "error" (json_of out)) in
        let asked = run_cli [ "prove"; "examples/even.lean"; "--no-such-option"; "--json" ] in
        equal_int "the same status" 2 asked.Proc.status;
        equal_string "nothing beside the document" "" asked.Proc.stderr;
        equal_string "the code" "MTH-CLI-0001" (Json.string_or "" "code" (error_of asked));
        equal_string "the message" "prove: unknown option --no-such-option" (Json.string_or "" "message" (error_of asked));
        let nowhere = run_cli [ "prov"; "--json" ] in
        equal_int "an unknown command" 2 nowhere.Proc.status;
        equal_string "its message" "unknown command 'prov' (did you mean 'prove'?)" (Json.string_or "" "message" (error_of nowhere));
        equal_string "its hint" "Run `maithic --help` for the list of commands." (Json.string_or "" "hint" (error_of nowhere)));
    case "P00-02-11 config show prints the effective configuration with its sources" (fun () ->
        let out = run_cli ~env:(clean_env @ [ ("MAITHIC_ROUNDS", "9") ]) [ "config"; "show" ] in
        contains "value" out.Proc.stdout "prove.rounds";
        contains "source" out.Proc.stdout "environment MAITHIC_ROUNDS";
        contains "mode" out.Proc.stdout "environment MAITHIC_GATEWAY_MODE");
    case "P10-05-06 auth status lists the providers and never prints a key" (fun () ->
        let key = "sk" ^ "-" ^ String.concat "" (List.init 8 (fun i -> Printf.sprintf "q%dW7" i)) in
        let out = run_cli ~env:(clean_env @ [ ("OPENAI_API_KEY", key) ]) [ "auth"; "status" ] in
        contains "provider" out.Proc.stdout "openai";
        contains "variable" out.Proc.stdout "OPENAI_API_KEY";
        check "the key is not printed" (not (Text.contains (out.Proc.stdout ^ out.Proc.stderr) (String.sub key 0 16)));
        contains "tail only" out.Proc.stdout (String.sub key (String.length key - 4) 4));
    case "P10-05-03 auth login stores a key read from standard input in a private file" (fun () ->
        let home = Fs.temp_dir "maithic-login" in
        let key = "xai" ^ "-" ^ String.concat "" (List.init 8 (fun i -> Printf.sprintf "k%dZ3" i)) in
        let env = [ ("MAITHIC_HOME", home); ("NO_COLOR", "1") ] in
        let out = run_cli ~env ~stdin:(key ^ "\n") [ "auth"; "login"; "--provider"; "xai" ] in
        equal_int "exit" 0 out.Proc.status;
        check "the key is not echoed" (not (Text.contains (out.Proc.stdout ^ out.Proc.stderr) key));
        equal_int "permissions" 0o600 ((Unix.stat Fs.(home / "credentials.toml")).Unix.st_perm land 0o777);
        contains "status" (run_cli ~env [ "auth"; "status" ]).Proc.stdout "credentials file";
        equal_int "logout" 0 (run_cli ~env [ "auth"; "logout"; "--provider"; "xai" ]).Proc.status;
        check "removed" (not (Text.contains (run_cli ~env [ "auth"; "status" ]).Proc.stdout "credentials file"));
        Fs.remove_tree home);
    case "P10-05-04 auth check confirms a provider answers, here from the mock provider" (fun () ->
        let out = run_cli [ "auth"; "check"; "--mode"; "mock" ] in
        equal_int "exit" 0 out.Proc.status;
        contains "answer" out.Proc.stdout "mock answered";
        equal_int "a live check without a key is a provider error" 5 (run_cli [ "auth"; "check"; "--mode"; "live" ]).Proc.status);
    case "check reports each theorem of a file with its status" (fun () ->
        let dir = Fs.temp_dir "maithic-check" in
        let file = Fs.(dir / "mixed.lean") in
        ignore
          (is_ok "write"
             (Fs.write file
                "theorem good (a b : Nat) : a + b = b + a := Nat.add_comm a b\n\ntheorem unfinished : 1 + 1 = 2 := by\n  sorry\n\naxiom shortcut : \xE2\x88\x80 n : Nat, n = 0\n\ntheorem cheated : 5 = 0 := shortcut 5\n"));
        let out = run_cli [ "check"; file; "--json" ] in
        equal_int "not all verified" 7 out.Proc.status;
        let theorems = Json.list_or_empty "theorems" (json_of out) in
        equal_strings "statuses" [ "cheated:failed"; "good:verified"; "unfinished:failed" ]
          (List.map (fun t -> Json.string_or "" "name" t ^ ":" ^ Json.string_or "" "status" t) theorems);
        let reasons name = String.concat " " (Json.strings (Json.List (Json.list_or_empty "reasons" (List.find (fun t -> Json.string_or "" "name" t = name) theorems)))) in
        contains "axiom" (reasons "cheated") "axioms outside the policy: shortcut";
        contains "sorry" (reasons "unfinished") "unfinished";
        ignore (is_ok "write" (Fs.write file "theorem good (a b : Nat) : a + b = b + a := Nat.add_comm a b\n"));
        equal_int "all verified" 0 (run_cli [ "check"; file ]).Proc.status;
        Fs.remove_tree dir);
    case "prove --write puts the verified proof into the file and leaves the rest alone" (fun () ->
        let dir = Fs.temp_dir "maithic-write" in
        let file = Fs.(dir / "even.lean") in
        ignore (is_ok "write" (Fs.write file (even ^ "\n-- a trailing comment\ntheorem other : True := trivial\n")));
        equal_int "exit" 0 (run_cli [ "prove"; file; "--write"; "--quiet" ]).Proc.status;
        let updated = is_ok "read" (Fs.read file) in
        contains "proof written" updated "(a + b) % 2 = 0 := by omega\n";
        contains "rest kept" updated "-- a trailing comment\ntheorem other : True := trivial\n";
        check "no sorry left" (not (Text.contains updated "sorry"));
        equal_int "the written file checks" 0 (run_cli [ "check"; file ]).Proc.status;
        Fs.remove_tree dir);
    case "bench run verifies every problem of the basics suite without a model" (fun () ->
        let out = run_cli [ "bench"; "run"; "--no-ai"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let j = json_of out in
        equal_string "schema" "maithic.bench.run/1" (Json.string_or "" "schema" j);
        equal_int "total" 24 (Json.int_or 0 "total" j);
        equal_int "verified" 24 (Json.int_or 0 "verified" j);
        check "every proof came from the portfolio" (List.for_all (fun p -> Json.string_or "" "proposer" p = "portfolio") (Json.list_or_empty "problems" j));
        equal_string "the committed manifest is the one the Lean package exports"
          (String.trim (read "bench/suites/manifest.json"))
          (String.trim (is_ok "manifest" (Proc.run ~cwd:root (path "lean/.lake/build/bin/maithic-bench") [ "manifest" ])).Proc.stdout)) ]

(* ---- what the command writes, against the published schemas ---- *)

let conforms rel j =
  let schema = is_ok rel (Json_schema.load (path rel)) in
  match Json_schema.conforms schema j with Ok () -> () | Error e -> fail (rel ^ ":\n" ^ e)

let contract_tests =
  [ case "a prove run conforms to its schema, with and without model calls" (fun () ->
        conforms "schemas/prove/run.schema.json" (json_of (run_cli [ "prove"; path "examples/even.lean"; "--json" ]));
        let assisted = json_of (run_cli [ "prove"; path "examples/triangle.lean"; "--provider"; "xai"; "--json" ]) in
        conforms "schemas/prove/run.schema.json" assisted;
        let records = Json.list_or_empty "records" (Option.get (Json.member "gateway" assisted)) in
        equal_int "one usage record per model call" 2 (List.length records);
        equal_strings "who answered" [ "xai"; "xai" ] (List.map (Json.string_or "" "provider") records);
        equal_strings "served from cassettes" [ "replay"; "replay" ] (List.map (Json.string_or "" "served_by") records));
    case "a prove run that fails still conforms to its schema" (fun () ->
        (* Without a model the triangle formula stays open: the portfolio has no induction. *)
        let out = run_cli ~env:(("MAITHIC_GATEWAY_MODE", "off") :: List.remove_assoc "MAITHIC_GATEWAY_MODE" clean_env)
            [ "prove"; path "examples/triangle.lean"; "--json" ] in
        equal_int "exit status for a theorem that is not verified" 7 out.Proc.status;
        let run = json_of out in
        conforms "schemas/prove/run.schema.json" run;
        check "the result says so" (Json.path [ "summary"; "verified" ] run = Some (Json.Int 0)));
    case "a check report conforms to its schema, for accepted and rejected files" (fun () ->
        conforms "schemas/check/report.schema.json" (json_of (run_cli [ "check"; path "lean/Maithic/Algo/Sort/Insertion.lean"; "--project"; path "lean"; "--json" ]));
        conforms "schemas/check/report.schema.json" (json_of (run_cli [ "check"; path "examples/triangle.lean"; "--json" ])));
    case "a benchmark run conforms to its schema" (fun () ->
        let out = Fs.(temp_dir "contract" / "run.json") in
        equal_int "exit" 0 (run_cli [ "bench"; "run"; "--no-ai"; "--out"; out; "--quiet" ]).Proc.status;
        conforms "schemas/bench/run.schema.json" (Json.parse_exn (is_ok "read" (Fs.read out)));
        conforms "schemas/bench/manifest.schema.json" (Json.parse_exn (read "bench/suites/manifest.json"))) ]

(* ---- the record of a search and its picture ---- *)

let view_tests =
  [ case "P01-06-03 a search on the Bridge is recorded with its goals as Lean prints them" (fun () ->
        with_bridge (fun b ->
            let env, _ = bridge_ok "header" (Bridge_client.load_header b []) in
            let proposer = Lean_search.vocabulary [ ("constructor", 0.5); ("exact hp", 0.5); ("exact hq", 0.5) ] in
            let statement = "theorem pair (p q : Prop) (hp : p) (hq : q) : p \xE2\x88\xA7 q" in
            let found = bridge_ok "search" (Lean_search.prove_statement ~proposer b ~env statement) in
            let record = Option.get found.Lean_search.record in
            equal_string "the statement" statement record.Search_record.statement;
            equal_strings "the goals" [ ""; "p \xE2\x88\xA7 q"; "p"; "q" ]
              (List.map (fun (n : Search_record.node) -> n.Search_record.goal) record.Search_record.nodes);
            check "the hypotheses of a goal"
              (List.mem "hp : p" (Option.get (Search_record.node record 2)).Search_record.hypotheses);
            equal_int "a step for every goal expanded" found.Lean_search.expansions (List.length record.Search_record.steps);
            equal_strings "the proof" [ "constructor"; "\xC2\xB7 exact hp"; "\xC2\xB7 exact hq" ] (Option.get record.Search_record.script);
            let schema = is_ok "schema" (Json_schema.load (path "schemas/prove/search.schema.json")) in
            check "the document conforms" (Json_schema.conforms schema (Search_record.to_json record) = Ok ());
            let nothing = bridge_ok "search" (Lean_search.prove_statement b ~env "theorem broken : (1 : Nat) = \"one\"") in
            check "a statement that opens no goal leaves no record" (nothing.Lean_search.record = None)));
    case "P01-06-04 the search on the example draws the pictures that the repository keeps" (fun () ->
        let dir = Fs.temp_dir "maithic-trees" in
        let found = run_cli ~env:[ ("MAITHIC_GATEWAY_MODE", "off"); ("NO_COLOR", "1") ] [ "prove"; "examples/double.lean"; "--no-ai"; "--search"; "--search-trees"; dir ] in
        equal_int "verified" 0 found.Proc.status;
        equal_string "the picture of the search that finds the proof" (read "fixtures/trees/found/double_eq.svg")
          (is_ok "picture" (Fs.read Fs.(dir / "double_eq.svg")));
        let stopped =
          run_cli ~env:[ ("MAITHIC_GATEWAY_MODE", "off"); ("NO_COLOR", "1") ]
            [ "prove"; "examples/double.lean"; "--no-ai"; "--search"; "--search-expansions"; "2"; "--search-trees"; dir ]
        in
        equal_int "not verified" (Exit_code.to_int Exit_code.Not_verified) stopped.Proc.status;
        equal_string "the picture of the search that runs out" (read "fixtures/trees/stopped/double_eq.svg")
          (is_ok "picture" (Fs.read Fs.(dir / "double_eq.svg")));
        Fs.remove_tree dir);
    case "P01-06-02 the tree command lists the steps of a recorded search and draws it" (fun () ->
        let dir = Fs.temp_dir "maithic-tree" in
        let out = run_cli [ "tree"; "fixtures/trees/found/double_eq.json"; "--steps"; "--svg"; Fs.(dir / "tree.svg") ] in
        equal_int "exit" 0 out.Proc.status;
        contains "statement" out.Proc.stdout "theorem double_eq (n : Nat) : double n = 2 * n";
        contains "summary" out.Proc.stdout "found: 4 goals expanded, 4 calls to Lean, 8 nodes";
        contains "proof" out.Proc.stdout "    induction n\n    \xC2\xB7 rfl\n    \xC2\xB7 simp [double]\n      omega";
        contains "first step" out.Proc.stdout "step 1  goal #1  double n = 2 * n";
        contains "the tactic of the proof is starred" out.Proc.stdout "  * induction n    leaves 2 goals: #2, #3";
        contains "a failure" out.Proc.stdout "    rfl            fails (tactic)";
        contains "the last step" out.Proc.stdout "  * omega          closes the goal";
        equal_string "the picture" (read "fixtures/trees/found/double_eq.svg") (is_ok "picture" (Fs.read Fs.(dir / "tree.svg")));
        let j = json_of (run_cli [ "tree"; "fixtures/trees/stopped/double_eq.json"; "--json" ]) in
        equal_string "as a document" "out of expansions" (Json.string_or "" "stop" j);
        let wrong = run_cli [ "tree"; "fixtures/runs/prove-even.json" ] in
        equal_int "another document is refused" (Exit_code.to_int Exit_code.Usage) wrong.Proc.status;
        contains "why" wrong.Proc.stderr "is not the record of a search";
        let missing = run_cli [ "tree" ] in
        equal_int "a file is needed" (Exit_code.to_int Exit_code.Usage) missing.Proc.status;
        Fs.remove_tree dir) ]

(* ---- what Lean knows about a file: extraction, the code graph, datasets and review ---- *)

let demo_lean = "fixtures/intel/lean/Demo.lean"
let same_file what kept made = equal_string what (read kept) (is_ok what (Fs.read made))

let insight_tests =
  [ case "P07-01-01 the Bridge describes a file: its declarations, the steps of its proofs, its gaps and what the lint rules find" (fun () ->
        with_bridge (fun b ->
            let e = bridge_ok "extract" (Bridge_client.extract ~file:demo_lean ~trace:true ~lint:true ~duplicates:true b (read demo_lean)) in
            check "the file elaborates: a sorry is a warning, not an error" (e.Extraction.ok && e.Extraction.errors = 0 && e.Extraction.warnings > 0);
            equal_strings "the declarations, in the order of the file"
              [ "double"; "double_eq"; "Bad_Name"; "open_one"; "uses_open"; "Point"; "instInhabitedPoint"; "shortcut"; "by_shortcut"; "Point3"; "term_gap" ]
              (List.map (fun (d : Extraction.declaration) -> d.Extraction.name) e.Extraction.declarations);
            let double = Option.get (Extraction.find e "double") and double_eq = Option.get (Extraction.find e "double_eq") in
            equal_string "documentation" "Doubles a number." (Option.get double.Extraction.doc);
            check "where a declaration begins and where its name stands" ((double.Extraction.line, double.Extraction.name_line) = (3, 4));
            equal_string "the statement as Lean prints it" "\xE2\x88\x80 (n : Nat), double n = 2 * n" double_eq.Extraction.type_;
            check "the statement refers to the definition" (List.mem "double" double_eq.Extraction.type_deps);
            check "the proof is kept as it stands in the file" (Text.starts_with ~prefix:"by\n  induction n with" (Option.get double_eq.Extraction.proof));
            equal_strings "the steps of the proof"
              [ "induction n with\n  | zero => rfl\n  | succ n ih =>\n    simp [double]\n    omega"; "rfl"; "simp [double]"; "omega" ]
              (List.filter_map (fun (s : Extraction.step) -> if s.Extraction.decl = "double_eq" then Some s.Extraction.tactic else None) e.Extraction.steps);
            let rfl = List.find (fun (s : Extraction.step) -> s.Extraction.tactic = "rfl") e.Extraction.steps in
            equal_strings "the goal a step met" [ "case zero\n\xE2\x8A\xA2 double 0 = 2 * 0" ] rfl.Extraction.goals_before;
            check "and the goals it left" (rfl.Extraction.goals_after = []);
            equal_strings "the gaps, each with the goal that is open"
              [ "open_one: n : Nat\n\xE2\x8A\xA2 n + 0 = n" ]
              (List.filter_map
                 (fun (h : Extraction.hole) -> if h.Extraction.goals = [] then None else Some (h.Extraction.hole_decl ^ ": " ^ String.concat " | " h.Extraction.goals))
                 e.Extraction.holes);
            check "what rests on sorry, itself or through what it uses"
              (List.for_all (fun name -> (Option.get (Extraction.find e name)).Extraction.uses_sorry) [ "open_one"; "uses_open"; "term_gap" ]
               && not double_eq.Extraction.uses_sorry);
            equal_strings "a theorem that states what another already states" [ "Nat.add_comm" ]
              (Option.get (Extraction.find e "Bad_Name")).Extraction.same_statement;
            equal_strings "the findings of the sorry rule"
              [ "open_one: the theorem contains sorry"; "uses_open: the theorem depends on sorry through open_one"; "term_gap: the theorem contains sorry" ]
              (List.filter_map
                 (fun (f : Extraction.finding) -> if f.Extraction.rule = "sorry" then Some (f.Extraction.declaration ^ ": " ^ f.Extraction.message) else None)
                 e.Extraction.findings);
            let only = bridge_ok "rules" (Bridge_client.extract ~lint:true ~rules:[ "naming" ] ~convention:[ ("theorems", "any") ] b (read demo_lean)) in
            equal_strings "rules can be chosen, and the convention changed" []
              (List.map (fun (f : Extraction.finding) -> f.Extraction.rule ^ " " ^ f.Extraction.declaration ^ ": " ^ f.Extraction.message) only.Extraction.findings);
            check "and steps are traced only when asked for" (only.Extraction.steps = []);
            (match Bridge_client.extract ~lint:true ~convention:[ ("theorems", "snakecase") ] b (read demo_lean) with
             | Error e -> contains "a convention that does not exist is refused" (Bridge_error.to_string e) "'snakecase' is not a naming convention for theorems"
             | Ok _ -> fail "a convention that does not exist was accepted");
            let schema = is_ok "schema" (Json_schema.load (path "schemas/bridge/extraction.schema.json")) in
            check "the document follows its schema" (Json_schema.conforms schema (Extraction.to_json e) = Ok ());
            let broken = bridge_ok "broken" (Bridge_client.extract ~trace:true b "theorem fine : 1 = 1 := rfl\ntheorem wrong : 1 = 2 := rfl\n") in
            check "a file with errors is not in order" (not broken.Extraction.ok);
            check "its errors are counted" (broken.Extraction.errors >= 1);
            check "what elaborated is described" (match Extraction.find broken "fine" with Some d -> not d.Extraction.uses_sorry | None -> false);
            (* Lean goes on after an error by putting sorry where the error was: the theorem is there, and is not finished. *)
            check "and what did not is not passed off as proved"
              (match Extraction.find broken "wrong" with Some d -> d.Extraction.uses_sorry | None -> true)));
    case "P07-02-02 intel extract and intel scan write the files the repository keeps" (fun () ->
        let dir = Fs.temp_dir "maithic-intel" in
        let extracted = run_cli [ "intel"; "extract"; "fixtures/intel/lean"; "lean/Maithic/Algo/Spec/Sorted.lean"; "lean/Maithic/Algo/Sort/Insertion.lean"; "lean/Maithic/Algo/Sort/Linear.lean"; "--project"; "lean"; "--trace"; "--out"; Fs.(dir / "extractions") ] in
        equal_int "extract" 0 extracted.Proc.status;
        contains "what was written" extracted.Proc.stdout "4 file(s), 41 declaration(s) written";
        List.iter
          (fun name -> same_file name ("fixtures/intel/extractions/" ^ name) Fs.(dir / "extractions" / name))
          [ "fixtures.intel.lean.Demo.json"; "Maithic.Algo.Sort.Insertion.json"; "Maithic.Algo.Sort.Linear.json"; "Maithic.Algo.Spec.Sorted.json"; "declarations.jsonl" ];
        let scanned = run_cli [ "intel"; "scan"; "fixtures/intel/lean"; "lean/Maithic/Algo/Spec/Sorted.lean"; "lean/Maithic/Algo/Sort/Insertion.lean"; "lean/Maithic/Algo/Sort/Linear.lean"; "--project"; "lean"; "--dir"; dir ] in
        equal_int "scan" 0 scanned.Proc.status;
        contains "summary" scanned.Proc.stdout "scanned 4 file(s): 41 declaration(s), 3 resting on sorry, 0 file(s) with errors";
        same_file "the graph" "fixtures/intel/graph.json" Fs.(dir / "graph.json");
        same_file "the health snapshot" "fixtures/intel/health.json" Fs.(dir / "health.json");
        let nothing = run_cli [ "intel"; "scan"; "docs"; "--dir"; dir ] in
        equal_int "a path without Lean files" (Exit_code.to_int Exit_code.Usage) nothing.Proc.status;
        Fs.remove_tree dir);
    case "P07-03-01 the questions about a scanned code base are answered from its graph" (fun () ->
        let ask args = run_cli ([ "intel" ] @ args @ [ "--dir"; "fixtures/intel" ]) in
        let shown = ask [ "show"; "Sorted" ] in
        equal_int "show" 0 shown.Proc.status;
        contains "found by the end of its name, with its place" shown.Proc.stdout "Maithic.Algo.Sorted  definition  lean/Maithic/Algo/Spec/Sorted.lean:31";
        contains "what it uses" shown.Proc.stdout "uses (0 here, 4 from outside): LE, LE.le, List, List.Pairwise";
        contains "what uses it" shown.Proc.stdout "used by (12): Maithic.Algo.Sorted.head_le";
        let as_json = json_of (ask [ "show"; "double_eq"; "--json" ]) in
        check "as a document" (Json.string_or "" "kind" as_json = "theorem" && Json.int_or 0 "line" as_json = 8 && not (Json.bool_or true "usesSorry" as_json));
        let deps = ask [ "deps"; "Maithic.Algo.insertionSort_idem" ] in
        contains "direct dependencies" deps.Proc.stdout "1         Maithic.Algo.sorted_insertionSort     theorem     lean/Maithic/Algo/Sort/Insertion.lean:83";
        contains "and what they depend on" deps.Proc.stdout "2         Maithic.Algo.insertSorted";
        let near = ask [ "deps"; "Maithic.Algo.insertionSort_idem"; "--depth"; "1" ] in
        check "a depth limit" (not (Text.contains near.Proc.stdout "\n2  "));
        let everything = ask [ "deps"; "double_eq"; "--all" ] in
        contains "with what comes from outside the scanned files" everything.Proc.stdout "outside";
        let dependents = ask [ "dependents"; "Maithic.Algo.insertSorted" ] in
        contains "what a change would reach" dependents.Proc.stdout "2         Maithic.Algo.insertionSort_idem       theorem     lean/Maithic/Algo/Sort/Insertion.lean:114";
        let health = ask [ "health" ] in
        contains "totals" health.Proc.stdout "4 file(s), 41 declaration(s), 26 documented, 3 resting on sorry";
        contains "a gap with its goal" health.Proc.stdout "fixtures/intel/lean/Demo.lean:18:2  open_one\n      n : Nat\n      \xE2\x8A\xA2 n + 0 = n";
        contains "a gap that is inherited" health.Proc.stdout "uses_open  (through open_one)";
        contains "an axiom of the code's own" health.Proc.stdout "shortcut  used by by_shortcut";
        let stats = ask [ "stats" ] in
        contains "nodes by kind" stats.Proc.stdout "nodes          declaration 251  file 4  module 5  namespace 3";
        contains "the most used declaration" stats.Proc.stdout "    12  Maithic.Algo.Sorted";
        let missing = ask [ "show"; "nothing_here" ] in
        equal_int "a name that is not in the graph" (Exit_code.to_int Exit_code.Usage) missing.Proc.status;
        contains "with its code" missing.Proc.stderr "MTH-INTEL-0003";
        let unscanned = run_cli [ "intel"; "health"; "--dir"; Fs.(Filename.get_temp_dir_name () / "maithic-no-scan") ] in
        equal_int "a folder that was never scanned" (Exit_code.to_int Exit_code.Usage) unscanned.Proc.status;
        contains "says how to make one" unscanned.Proc.stderr "`maithic intel scan PATH` writes it");
    case "P11-03-03 corpus harvest traces a project into a dataset, the same one the repository keeps" (fun () ->
        let dir = Fs.temp_dir "maithic-corpus" in
        let out =
          run_cli [ "corpus"; "harvest"; "fixtures/intel/lean"; "lean/Maithic/Algo/Spec/Sorted.lean"; "lean/Maithic/Algo/Sort/Insertion.lean"; "lean/Maithic/Algo/Sort/Linear.lean"; "--project"; "lean"; "--out"; dir; "--name"; "demo"; "--revision"; "fixture" ]
        in
        equal_int "exit" 0 out.Proc.status;
        contains "what was harvested" out.Proc.stdout "harvested 4 file(s) at revision fixture:\n  28 theorem record(s), 139 step record(s), 613 premise record(s), 88 pair record(s)";
        same_file "the manifest" "fixtures/corpus/demo/manifest.json" Fs.(dir / "manifest.json");
        let manifest = is_ok "manifest" (Maithic_corpus.Corpus_store.read_manifest dir) in
        List.iter
          (fun (s : Maithic_corpus.Corpus_store.shard) -> same_file s.Maithic_corpus.Corpus_store.file ("fixtures/corpus/demo/" ^ s.Maithic_corpus.Corpus_store.file) Fs.(dir / s.Maithic_corpus.Corpus_store.file))
          manifest.Maithic_corpus.Corpus_store.shards;
        let record = Json.parse_exn (is_ok "record" (Fs.read Fs.(dir / "harvest.json"))) in
        conforms "schemas/corpus/harvest.schema.json" record;
        equal_string "the record names the toolchain that traced the files" Version.lean_toolchain (Json.string_or "" "toolchain" record);
        equal_strings "and the outcome for each file" [ "ok"; "ok"; "ok"; "ok" ] (List.map (Json.string_or "" "outcome") (Json.list_or_empty "files" record));
        let sound = run_cli [ "corpus"; "verify"; dir ] in
        equal_int "the dataset verifies" 0 sound.Proc.status;
        contains "and says what it holds" sound.Proc.stdout "demo 1 is sound: 28 theorem record(s)";
        (* One byte more in a shard, and the dataset no longer verifies. *)
        let shard = Fs.(dir / (List.hd manifest.Maithic_corpus.Corpus_store.shards).Maithic_corpus.Corpus_store.file) in
        ignore (is_ok "change" (Fs.write shard (is_ok "shard" (Fs.read shard) ^ "\n")));
        let changed = run_cli [ "corpus"; "verify"; dir ] in
        equal_int "a changed shard" (Exit_code.to_int Exit_code.Not_verified) changed.Proc.status;
        contains "is named" changed.Proc.stdout "does not have the hash the manifest states";
        (* A harvest goes on past a file that does not elaborate, and records it. *)
        let sources = Fs.temp_dir "maithic-sources" in
        ignore (is_ok "good" (Fs.write Fs.(sources / "Good.lean") "theorem two : 1 + 1 = 2 := by\n  rfl\n"));
        ignore (is_ok "bad" (Fs.write Fs.(sources / "Bad.lean") "import No.Such.Module\ntheorem lost : True := trivial\n"));
        let partial = run_cli [ "corpus"; "harvest"; sources; "--out"; Fs.(dir / "partial"); "--json" ] in
        equal_int "still a dataset" 0 partial.Proc.status;
        let outcomes = List.map (fun o -> (Fs.basename (Json.string_or "" "file" o), Json.string_or "" "outcome" o)) (Json.list_or_empty "files" (json_of partial)) in
        check "one file traced, one not" (List.sort compare outcomes = [ ("Bad.lean", "failed"); ("Good.lean", "ok") ]);
        equal_int "the theorem of the file that was traced" 1 (Json.int_or 0 "theorem" (Option.get (Json.member "records" (json_of partial))));
        let none = run_cli [ "corpus"; "harvest"; "docs"; "--out"; Fs.(dir / "none") ] in
        equal_int "no Lean file among the paths" (Exit_code.to_int Exit_code.Usage) none.Proc.status;
        Fs.remove_tree sources;
        Fs.remove_tree dir);
    case "P03-04-02 review reports only the edits that Lean accepted, and fails on unfinished work" (fun () ->
        let dir = Fs.temp_dir "maithic-review" in
        let suggested = run_cli [ "review"; "fixtures/review/lean/Simplify.lean"; "--suggest" ] in
        equal_int "notes and warnings do not fail a review" 0 suggested.Proc.status;
        contains "a proof that one call of automation replaces" suggested.Proc.stdout
          "fixtures/review/lean/Simplify.lean:4  info     automation  le_succ_of_le'\n    the proof can be replaced by `by omega`; Lean accepts it";
        contains "the line of the theorem" suggested.Proc.stdout "       4 | theorem le_succ_of_le' (a b : Nat) (h : a \xE2\x89\xA4 b) : a \xE2\x89\xA4 b + 1 := by";
        contains "a step that is not needed" suggested.Proc.stdout "one step is not needed: Lean accepts the proof without `have zero : tri 0 = 0 := rfl`";
        contains "the proof that is left" suggested.Proc.stdout "    + by\n    +   induction n with\n    +   | zero => rfl\n    +   | succ k ih => simp only [tri]; grind";
        contains "a suppression without a reason" suggested.Proc.stdout "a suppression needs a reason in parentheses";
        contains "the summary" suggested.Proc.stdout "1 file(s), 4 finding(s): 0 error(s), 1 warning(s), 3 note(s), 1 suppressed";
        let plain = run_cli [ "review"; "fixtures/review/lean/Simplify.lean" ] in
        check "without --suggest no edit is proposed" (not (Text.contains plain.Proc.stdout "automation"));
        (* The report of both demonstration files is the one the repository keeps. *)
        let report = Fs.(dir / "report.json") in
        let both = run_cli [ "review"; "fixtures/intel/lean"; "fixtures/review/lean"; "--suggest"; "--format"; "json"; "--out"; report ] in
        equal_int "unfinished work fails the review" (Exit_code.to_int Exit_code.Not_verified) both.Proc.status;
        same_file "the report" "fixtures/review/report.json" report;
        conforms "schemas/review/report.schema.json" (Json.parse_exn (read "fixtures/review/report.json"));
        let markdown = run_cli [ "review"; demo_lean; "--format"; "markdown" ] in
        contains "as Markdown" markdown.Proc.stdout "| 17 | error | sorry | `open_one` | the theorem contains sorry |";
        let strict = run_cli [ "review"; "fixtures/review/lean/Simplify.lean"; "--profile"; "strict" ] in
        equal_int "under the strict profile nothing here is an error yet" 0 strict.Proc.status;
        contains "but missing documentation is a warning" strict.Proc.stdout "warning  docstring  tri_one";
        let minimal = run_cli [ "review"; demo_lean; "--profile"; "minimal" ] in
        check "the minimal profile keeps to unfinished work and axioms"
          (Text.contains minimal.Proc.stdout "sorry  open_one" && not (Text.contains minimal.Proc.stdout "docstring"));
        let unknown = run_cli [ "review"; demo_lean; "--profile"; "nope" ] in
        equal_int "a profile that does not exist" (Exit_code.to_int Exit_code.Usage) unknown.Proc.status;
        contains "with its code" unknown.Proc.stderr "MTH-REVIEW-0001";
        Fs.remove_tree dir) ]

(* ---- counterexamples, each confirmed by Lean ---- *)

let conjectures = "examples/conjectures.lean"

(* A report without its timings, which differ from run to run. *)
let rec timeless = function
  | Json.Obj members ->
    Json.Obj
      (List.filter_map
         (fun (k, v) -> if k = "seconds" && (match v with Json.Float _ | Json.Int _ -> true | _ -> false) then None else Some (k, timeless v))
         members)
  | Json.List items -> Json.List (List.map timeless items)
  | other -> other

let refute_tests =
  [ case "P14-02-01 Lean confirms a counterexample by a proof that the statement is false, and nothing less counts" (fun () ->
        with_bridge (fun b ->
            let source = read conjectures in
            let decl = is_ok "find" (Lean_source.find source "square_monotone") in
            let _, before = bridge_ok "prelude" (Bridge_client.check_file ~keep:true b (Maithic_refute.Refute_probe.with_library decl.Lean_source.prelude)) in
            let _, after = bridge_ok "declare" (Bridge_client.run_command ~keep:true b ~env:(Option.get before) (Maithic_refute.Refute_probe.declaration decl)) in
            let after = Option.get after in
            let name = Maithic_refute.Refute_witness.name_of "square_monotone" in
            let rejected error = { Maithic_refute.Refute_witness.accepted = false; uses_sorry = false; axioms = []; error } in
            let lean_check ~untrusted lean =
              match bridge_ok "try" (Bridge_client.try_candidates ~names:[ name ] b ~env:after [ { Bridge_types.source = lean; untrusted } ]) with
              | [ { Bridge_types.report = Some report; _ } ] when report.Bridge_types.ok ->
                let d = List.find (fun (d : Bridge_types.decl) -> d.Bridge_types.name = name) report.Bridge_types.decls in
                { Maithic_refute.Refute_witness.accepted = d.Bridge_types.found; uses_sorry = d.Bridge_types.uses_sorry; axioms = d.Bridge_types.axioms; error = "" }
              | [ { Bridge_types.report = Some report; _ } ] -> rejected (Bridge_types.error_text report)
              | [ { Bridge_types.rejection; _ } ] -> rejected ("guard: " ^ rejection)
              | _ -> fail "one candidate, one report"
            in
            let confirm = Maithic_refute.Refute_witness.confirm ~check:lean_check ~theorem:"square_monotone" in
            let c = is_ok "a real counterexample" (confirm [ ("x", "-1"); ("y", "0") ]) in
            equal_string "closed by the first tactic" "decide" c.Maithic_refute.Refute_witness.closer;
            check "on no axiom at all" (c.Maithic_refute.Refute_witness.axioms = []);
            (* Values at which the statement holds refute nothing, whoever proposed them. *)
            check "a point where the statement holds" (Result.is_error (confirm [ ("x", "1"); ("y", "2") ]));
            (* The hypothesis is part of what is checked: at x = 3, y = 1 it fails, so the statement says nothing there. *)
            check "a point that the hypothesis excludes" (Result.is_error (confirm [ ("x", "3"); ("y", "1") ]));
            check "a variable the statement does not have" (Result.is_error (confirm [ ("z", "0") ]));
            (* What comes from outside goes through the guard before Lean sees it. *)
            contains "a value that carries a command" (is_error "guard" (confirm [ ("x", "0)\n#eval IO.println 1 --"); ("y", "0") ])) "guard:"));
    case "P14-02-02 maithic refute tells false, empty and standing statements apart" (fun () ->
        let dir = Fs.temp_dir "maithic-refute" in
        let out = run_cli [ "refute"; conjectures; "--method"; "test,vacuity"; "--out"; Fs.(dir / "report.json") ] in
        equal_int "a refuted statement decides the exit status" (Exit_code.to_int Exit_code.Refuted) out.Proc.status;
        contains "a counterexample" out.Proc.stdout "  test      falsified at case 5 with seed 0: a := 1, b := 0";
        contains "confirmed" out.Proc.stdout "  Lean      accepts the refutation, closed by decide; it rests on no axiom";
        contains "with its proof" out.Proc.stdout
          "    theorem _root_.maithic_refutation_of_sum_le_product : \xC2\xAC type_of% @sum_le_product := by\n      intro h\n      have h' := h (a := 1) (b := 0)\n      revert h'\n      decide";
        contains "a hypothesis is kept in the check" out.Proc.stdout "have h' := h (x := -1) (y := 0)";
        contains "rationals" out.Proc.stdout "closed by decide +kernel";
        contains "a statement sampling cannot reach" out.Proc.stdout "  test      gave up with seed 0: 1000 cases left out by the hypotheses, 0 passed";
        contains "is found empty" out.Proc.stdout "the statement is true and says nothing: the hypotheses h\xE2\x82\x81, h\xE2\x82\x82 contradict each other (shown by omega)";
        contains "a true statement stands" out.Proc.stdout "  standing  nothing was found, which proves nothing";
        (* A name Lean does not know becomes a variable: the statement elaborates, says something else, and is refuted as it is. *)
        contains "a slip of the key" out.Proc.stdout "theorem pred_lt (n : Nat) (h : 0 < n) : n - 1 < m\n  test      falsified";
        contains "summary" out.Proc.stdout "6 statements: 4 refuted, 1 vacuous, 0 with a counterexample Lean did not confirm, 1 standing";
        let written = Json.parse_exn (is_ok "report" (Fs.read Fs.(dir / "report.json"))) in
        conforms "schemas/refute/report.schema.json" written;
        (* One theorem, chosen by name, with a budget of its own. *)
        let one = run_cli [ "refute"; conjectures; "--theorem"; "reverse_append"; "--method"; "test"; "--rounds"; "2"; "--cases"; "40"; "--seed"; "11"; "--json" ] in
        equal_int "nothing found is a success of the search" 0 one.Proc.status;
        let result = List.hd (Json.list_or_empty "results" (json_of one)) in
        equal_strings "the rounds, each with the next seed" [ "passed 40 cases with seed 11"; "passed 40 cases with seed 12" ]
          (List.map (Json.string_or "" "detail") (Json.list_or_empty "steps" result));
        equal_string "verdict" "not-refuted" (Json.string_or "" "verdict" result);
        (* A theorem with a proof can be examined by name; a file without unfinished theorems needs one. *)
        let proved = run_cli [ "refute"; "examples/double.lean"; "--theorem"; "double_eq"; "--method"; "test" ] in
        equal_int "a proved theorem stands" 0 proved.Proc.status;
        let none = run_cli [ "refute"; "lean/Maithic/Algo/Spec/Sorted.lean"; "--project"; "lean" ] in
        equal_int "nothing unfinished" (Exit_code.to_int Exit_code.Usage) none.Proc.status;
        contains "says what to do" none.Proc.stderr "has no theorem with an unfinished proof; name one with --theorem";
        let missing = run_cli [ "refute"; conjectures; "--theorem"; "nope" ] in
        contains "a theorem that is not there" missing.Proc.stderr "MTH-REFUTE-0001";
        let odd = run_cli [ "refute"; conjectures; "--method"; "magic" ] in
        equal_int "a method that does not exist" (Exit_code.to_int Exit_code.Usage) odd.Proc.status;
        let zero = run_cli [ "refute"; conjectures; "--rounds"; "0" ] in
        contains "a budget that is none" zero.Proc.stderr "--rounds needs a whole number, 1 or more";
        (* A statement that does not elaborate is not examined, and the reason is Lean's. *)
        let broken = Fs.(dir / "Broken.lean") in
        ignore (is_ok "write" (Fs.write broken "theorem broken (a : Nat) : a + \"one\" = a := by\n  sorry\n"));
        let failed = run_cli [ "refute"; broken ] in
        equal_int "a statement with an error" (Exit_code.to_int Exit_code.Failure) failed.Proc.status;
        contains "with its code" failed.Proc.stderr "MTH-REFUTE-0003";
        Fs.remove_tree dir);
    case "P14-02-02 a point from a solver is a candidate, and the recorded report is reproduced" (fun () ->
        need_julia ();
        let solved = run_cli [ "refute"; conjectures; "--theorem"; "within_budget"; "--method"; "oracle" ] in
        equal_int "refuted" (Exit_code.to_int Exit_code.Refuted) solved.Proc.status;
        contains "the solver's point" solved.Proc.stdout "  oracle    a solver found a point where the statement fails: x := 3, y := 0";
        contains "confirmed by Lean" solved.Proc.stdout "have h' := h (x := 3) (y := 0)";
        let whole = run_cli [ "refute"; conjectures; "--json" ] in
        equal_int "exit" (Exit_code.to_int Exit_code.Refuted) whole.Proc.status;
        check "the report the repository keeps, timings aside"
          (Json.equal (timeless (json_of whole)) (timeless (Json.parse_exn (read "fixtures/refute/conjectures.json"))))) ]

(* ---- the guide: a trained model, served by Julia, in a search over Lean ---- *)

let guide_model = "fixtures/guide/library/model.json"

let guide_tests =
  let open Maithic_guide in
  let expansions_of file = Json.int_or (-1) "expansions" (Json.parse_exn (is_ok file (Fs.read file))) in
  let guide_of j = Option.get (Json.member "guide" j) in
  [ case "P12-05-01 the service answers the three questions of a search about the recorded model" (fun () ->
        need_julia ();
        let engine = is_ok "locate" (Guide_service.locate ~cwd:root ()) in
        let client = is_ok "connect" (Guide_service.connect engine ~model:(path guide_model)) in
        let summary = is_ok "load" (Guide_model.load (path guide_model)) in
        let info = Guide_client.info client in
        equal_int "the model it says it serves is the model of the file" summary.Guide_model.dim info.Guide_client.dim;
        equal_int "its tokens" summary.Guide_model.vocabulary info.Guide_client.vocabulary;
        equal_int "its templates" (List.length summary.Guide_model.templates) info.Guide_client.classes;
        let state = "n : Nat\n\xE2\x8A\xA2 double n = 2 * n" in
        let candidates = [ "rfl"; "omega"; "simp [double]"; "induction n"; "simp [triple]" ] in
        let ranking = is_ok "tactic" (Guide_client.tactic client ~state candidates) in
        equal_int "a prior for each candidate" 5 (List.length ranking.Guide_client.priors);
        check "the priors add up to one" (Float.abs (List.fold_left ( +. ) 0.0 ranking.Guide_client.priors -. 1.0) < 1e-9);
        equal_strings "how the candidates were read" [ "rfl"; "omega"; "simp [_]"; "induction _"; "simp [_]" ] ranking.Guide_client.templates;
        check "two candidates of one template get the same prior" (List.nth ranking.Guide_client.priors 2 = List.nth ranking.Guide_client.priors 4);
        let value = is_ok "value" (Guide_client.value client ~state) in
        check "a value between zero and one" (value >= 0.0 && value <= 1.0);
        let scores = is_ok "premise" (Guide_client.premise client ~state [ "Nat.mul_comm"; "List.length_append" ]) in
        check "a premise that shares words with the goal scores above one that shares none" (List.nth scores 0 > List.nth scores 1);
        (* The same question, the same answer: the service keeps nothing between requests. *)
        check "asked again" ((is_ok "again" (Guide_client.tactic client ~state candidates)).Guide_client.priors = ranking.Guide_client.priors);
        Guide_client.close client);
    case "P12-05-02 a guided search reaches the proof in fewer expansions, and the proof is checked like any other" (fun () ->
        need_julia ();
        let dir = Fs.temp_dir "maithic-guided" in
        let plain = run_cli [ "prove"; "examples/guided.lean"; "--no-ai"; "--search"; "--search-trees"; Fs.(dir / "plain"); "--json" ] in
        let guided = run_cli [ "prove"; "examples/guided.lean"; "--no-ai"; "--guide"; guide_model; "--search-trees"; Fs.(dir / "guided"); "--json" ] in
        equal_int "without a guide" 0 plain.Proc.status;
        equal_int "with a guide" 0 guided.Proc.status;
        List.iter
          (fun out ->
            let result = List.hd (Json.list_or_empty "results" (json_of out)) in
            equal_string "verified" "verified" (Json.string_or "" "status" result);
            equal_string "by the search" "search" (Json.string_or "" "proposer" result);
            conforms "schemas/prove/run.schema.json" (json_of out))
          [ plain; guided ];
        check "a run without a guide does not speak of one" (Json.member "guide" (json_of plain) = None);
        let guide = guide_of (json_of guided) in
        equal_string "the model, as it was named" guide_model (Json.string_or "" "model" guide);
        equal_string "the service answered to the end" "up" (Json.string_or "" "status" guide);
        let without = expansions_of Fs.(dir / "plain" / "found_iff_mem.json") and with_guide = expansions_of Fs.(dir / "guided" / "found_iff_mem.json") in
        equal_int "one question for every goal that was expanded" with_guide (Json.int_or 0 "asked" guide);
        equal_int "each of them answered" with_guide (Json.int_or 0 "answered" guide);
        check (Printf.sprintf "fewer goals were expanded with the guide (%d) than without (%d)" with_guide without) (with_guide < without);
        (* In text, the run ends with a line about its guide. *)
        let text = run_cli [ "prove"; "examples/guided.lean"; "--no-ai"; "--guide"; guide_model ] in
        contains "the line" text.Proc.stdout (Printf.sprintf "the guide %s answered %d of %d question(s)" guide_model with_guide with_guide);
        Fs.remove_tree dir);
    case "P12-05-03 without its guide the search goes on as if it had none, and the report says why" (fun () ->
        (* A model that is not there, and a Julia that is not there: neither needs Julia to be installed. *)
        let dir = Fs.temp_dir "maithic-unguided" in
        let plain = run_cli [ "prove"; "examples/double.lean"; "--no-ai"; "--search"; "--search-trees"; Fs.(dir / "plain"); "--json" ] in
        let expected = expansions_of Fs.(dir / "plain" / "double_eq.json") in
        let down what env args reason =
          let out = run_cli ~env:(clean_env @ env) ([ "prove"; "examples/double.lean"; "--no-ai"; "--search-trees"; Fs.(dir / what); "--json" ] @ args) in
          equal_int (what ^ ": the theorem is proved all the same") 0 out.Proc.status;
          conforms "schemas/prove/run.schema.json" (json_of out);
          let guide = guide_of (json_of out) in
          equal_string (what ^ ": the guide is down") "down" (Json.string_or "" "status" guide);
          contains (what ^ ": the reason") (Json.string_or "" "reason" guide) reason;
          equal_int (what ^ ": nothing was answered by a service") 0 (Json.int_or (-1) "answered" guide);
          equal_int (what ^ ": every goal was asked about") expected (Json.int_or (-1) "asked" guide);
          equal_int (what ^ ": the search is the search without a guide") expected (expansions_of Fs.(dir / what / "double_eq.json"));
          check (what ^ ": the same proof")
            (Json.equal
               (Option.get (Json.member "proof" (List.hd (Json.list_or_empty "results" (json_of out)))))
               (Option.get (Json.member "proof" (List.hd (Json.list_or_empty "results" (json_of plain))))))
        in
        down "no-model" [] [ "--guide"; "fixtures/guide/nowhere.json" ] (if Proc.find_in_path "julia" = None then "is not on the path" else "there is no model at fixtures/guide/nowhere.json");
        down "no-julia" [ ("MAITHIC_JULIA", "no-such-julia") ] [ "--guide"; guide_model ] "the Julia executable \"no-such-julia\" is not on the path";
        (* A model set in the configuration is used without the option, and a run that never searches never starts it. *)
        let idle = run_cli ~env:(clean_env @ [ ("MAITHIC_GUIDE_MODEL", path guide_model) ]) [ "prove"; "examples/even.lean"; "--json" ] in
        equal_int "proved by the portfolio" 0 idle.Proc.status;
        equal_string "the guide was not needed" "idle" (Json.string_or "" "status" (guide_of (json_of idle)));
        conforms "schemas/prove/run.schema.json" (json_of idle);
        Fs.remove_tree dir);
    case "P12-05-01 guide info reads a model without running it, and guide rank asks it about a goal" (fun () ->
        let info = run_cli [ "guide"; "info"; guide_model; "--top"; "3" ] in
        equal_int "info" 0 info.Proc.status;
        contains "the shape" info.Proc.stdout "tactic model  mean pooling, vectors of 32, reads up to 128 tokens";
        contains "the first template" info.Proc.stdout "exact _";
        let summary = json_of (run_cli [ "guide"; "info"; guide_model; "--json" ]) in
        equal_int "as a document" 128 (Json.int_or 0 "max_length" summary);
        let absent = run_cli [ "guide"; "info"; "fixtures/guide/nowhere.json" ] in
        equal_int "a file that is not there" (Exit_code.to_int Exit_code.Usage) absent.Proc.status;
        contains "its code" absent.Proc.stderr "MTH-GUIDE-0002";
        let odd = run_cli [ "guide"; "info"; "fixtures/guide/library/run.json" ] in
        contains "a file that is no model" odd.Proc.stderr "is not a model";
        equal_int "a goal is needed" (Exit_code.to_int Exit_code.Usage) (run_cli [ "guide"; "rank"; guide_model; "omega" ]).Proc.status;
        need_julia ();
        (* The candidates of the search for a theorem of a file, with what the guide makes of their priors. *)
        let searched = run_cli [ "guide"; "rank"; guide_model; "--file"; "examples/double.lean"; "--theorem"; "double_eq"; "--json" ] in
        equal_int "rank" 0 searched.Proc.status;
        conforms "schemas/guide/ranking.schema.json" (json_of searched);
        equal_string "the goal, as Lean prints it" "n : Nat\n\xE2\x8A\xA2 double n = 2 * n" (Json.string_or "" "goal" (json_of searched));
        let rows = Json.list_or_empty "candidates" (json_of searched) in
        equal_strings "the candidates of the search"
          (List.sort compare [ "rfl"; "assumption"; "omega"; "simp"; "simp_all"; "decide"; "grind"; "induction n"; "simp [double]" ])
          (List.sort compare (List.map (Json.string_or "" "tactic") rows));
        check "each with the prior of the search and the prior after the guide"
          (List.for_all (fun r -> Json.member "search_prior" r <> None && Json.member "guided_prior" r <> None) rows);
        check "no prior grows under the guide"
          (List.for_all
             (fun r -> Option.bind (Json.member "guided_prior" r) Json.float_opt <= Option.bind (Json.member "search_prior" r) Json.float_opt)
             rows);
        check "the model's favourite keeps its prior"
          (List.exists (fun r -> Json.member "guided_prior" r = Json.member "search_prior" r) rows);
        (* Tactics named on the command line, for a goal given as text. *)
        let named = run_cli [ "guide"; "rank"; guide_model; "--goal"; "h : a \xE2\x89\xA4 b\n\xE2\x8A\xA2 a \xE2\x89\xA4 b + 1"; "omega"; "exact h"; "simp [foo]" ] in
        equal_int "named tactics" 0 named.Proc.status;
        contains "the table" named.Proc.stdout "read as";
        contains "a template" named.Proc.stdout "simp [_]";
        contains "the value" named.Proc.stdout "the model's estimate that the next tactic closes a goal";
        (* Without tactics, the templates the model knows are ranked. *)
        let templates = json_of (run_cli [ "guide"; "rank"; guide_model; "--goal"; "\xE2\x8A\xA2 True"; "--json" ]) in
        conforms "schemas/guide/ranking.schema.json" templates;
        equal_int "one row for each template but the class of all others"
          (List.length (is_ok "load" (Guide_model.load (path guide_model))).Guide_model.templates - 1)
          (List.length (Json.list_or_empty "candidates" templates)));
    case "P12-03-04 guide train writes a model and the record of its run from a dataset" (fun () ->
        need_julia ();
        let dir = Fs.temp_dir "maithic-train" in
        let out = run_cli [ "guide"; "train"; "fixtures/corpus/demo"; dir; "--seed"; "3"; "--epochs"; "3"; "--dim"; "8"; "--name"; "demo" ] in
        equal_int "exit" 0 out.Proc.status;
        contains "what was trained" out.Proc.stdout "trained demo on demo:";
        contains "what it was measured against" out.Proc.stdout "always answering with the most frequent templates";
        let model = is_ok "the model" (Guide_model.load Fs.(dir / "model.json")) in
        equal_int "of the size asked for" 8 model.Guide_model.dim;
        let record = Json.parse_exn (is_ok "the record" (Fs.read Fs.(dir / "run.json"))) in
        conforms "schemas/guide/run.schema.json" record;
        conforms "schemas/guide/model.schema.json" (Json.parse_exn (is_ok "the model" (Fs.read Fs.(dir / "model.json"))));
        equal_int "three epochs" 3 (List.length (Json.list_or_empty "epochs" record));
        equal_int "the weights of the model" model.Guide_model.parameters (Json.int_or 0 "parameters" record);
        (* The same command writes the same files. *)
        let again = Fs.temp_dir "maithic-train" in
        ignore (run_cli [ "guide"; "train"; "fixtures/corpus/demo"; again; "--seed"; "3"; "--epochs"; "3"; "--dim"; "8"; "--name"; "demo"; "--quiet" ]);
        equal_string "the model, byte for byte" (is_ok "first" (Fs.read Fs.(dir / "model.json"))) (is_ok "second" (Fs.read Fs.(again / "model.json")));
        equal_string "and the record" (is_ok "first" (Fs.read Fs.(dir / "run.json"))) (is_ok "second" (Fs.read Fs.(again / "run.json")));
        let nothing = run_cli [ "guide"; "train"; "fixtures/guide"; dir ] in
        equal_int "a directory that is no dataset" (Exit_code.to_int Exit_code.Usage) nothing.Proc.status;
        contains "why" nothing.Proc.stderr "is not a dataset";
        let wrong = run_cli [ "guide"; "train"; "fixtures/corpus/demo"; dir; "--pooling"; "max" ] in
        equal_int "an option the training refuses" (Exit_code.to_int Exit_code.Failure) wrong.Proc.status;
        contains "its code" wrong.Proc.stderr "MTH-GUIDE-0004";
        contains "what the training said" wrong.Proc.stderr "there is no pooling called max";
        Fs.remove_tree dir;
        Fs.remove_tree again) ]

(* ---- repair: rules, a replayed model and the prover, on a file that stopped compiling ---- *)

let upgrade = "fixtures/mend/lean/Upgrade.lean"
let verbose = "fixtures/mend/lean/Verbose.lean"
let renames = "fixtures/mend/renames.toml"

(* A document without what differs from run to run: the account of the gateway, with its times. *)
let without_account = function Json.Obj members -> Json.Obj (List.filter (fun (k, _) -> k <> "gateway") members) | other -> other

let mend_tests =
  let open Maithic_mend in
  let stages j = List.map (fun r -> (Json.string_or "" "declaration" r, Json.string_or "" "stage" r)) (Json.list_or_empty "repairs" j) in
  let summary j key = Json.int_or (-1) key (Option.get (Json.member "summary" j)) in
  let copy_of file =
    let dir = Fs.temp_dir "maithic-mend" in
    let copy = Fs.(dir / Filename.basename file) in
    ignore (is_ok "copy" (Fs.write copy (read file)));
    (dir, copy)
  in
  [ case "P17-01-01 mend scan gives each file to Lean and collects what is broken, by theorem and class" (fun () ->
        let out = run_cli [ "mend"; "scan"; upgrade; "--json" ] in
        equal_int "a file with errors" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let scan = json_of out in
        conforms "schemas/mend/scan.schema.json" scan;
        check "the scan the repository keeps" (Json.equal scan (Json.parse_exn (read "fixtures/mend/scan.json")));
        let breakages = Json.list_or_empty "breakages" (List.hd (Json.list_or_empty "files" scan)) in
        equal_strings "the theorems that are broken" [ "sum_comm"; "twice_add_zero"; "le_succ_self"; "twice_add"; "add_zero_eq" ] (List.map (Json.string_or "" "declaration") breakages);
        equal_strings "each in its class" [ "unknown-identifier"; "deprecated"; "suggestion"; "unsolved-goals"; "type-mismatch" ] (List.map (Json.string_or "" "class") breakages);
        equal_strings "with the severity Lean gave it" [ "error"; "warning"; "info"; "error"; "error" ] (List.map (Json.string_or "" "severity") breakages);
        equal_string "the name that is gone" "Nat.add_commutes" (Json.string_or "" "detail" (List.hd breakages));
        contains "the context is the theorem" (Json.string_or "" "context" (List.hd breakages)) "theorem sum_comm (a b : Nat) : a + b = b + a := by";
        (* In text, and for a directory with a file that has nothing broken. *)
        let text = run_cli [ "mend"; "scan"; "fixtures/mend/lean"; "examples/even.lean" ] in
        contains "the table" text.Proc.stdout "sum_comm        error     unknown-identifier  Unknown constant `Nat.add_commutes`";
        contains "a suggestion" text.Proc.stdout "le_succ_self    info      suggestion          Try this: exact Nat.le_add_right n 1";
        contains "a file that works" text.Proc.stdout "fixtures/mend/lean/Verbose.lean  nothing is broken";
        contains "the sum" text.Proc.stdout "3 file(s), 5 breakage(s): 1 unknown-identifier, 1 deprecated, 1 unsolved-goals, 1 type-mismatch, 1 suggestion";
        equal_int "a healthy file" 0 (run_cli [ "mend"; "scan"; "examples/double.lean" ]).Proc.status;
        equal_int "a path is needed" (Exit_code.to_int Exit_code.Usage) (run_cli [ "mend"; "scan" ]).Proc.status);
    case "P17-02-03 the rules repair what they can, and each edit that stays was compiled by Lean" (fun () ->
        let out = run_cli [ "mend"; "repair"; upgrade; "--renames"; renames; "--no-ai"; "--no-prove"; "--json" ] in
        equal_int "proofs are left" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let report = json_of out in
        conforms "schemas/mend/report.schema.json" report;
        check "three by a rule, two left for other remedies"
          (stages report = [ ("sum_comm", "rule"); ("twice_add_zero", "rule"); ("le_succ_self", "rule"); ("twice_add", "none"); ("add_zero_eq", "none") ]);
        equal_strings "which rule" [ "rename"; "rename"; "suggestion" ] (List.filter_map (fun r -> Option.bind (Json.member "rule" r) Json.string_opt) (Json.list_or_empty "repairs" report));
        let first = List.hd (Json.list_or_empty "repairs" report) in
        contains "the theorem after the rename" (Json.string_or "" "after" first) "exact Nat.add_comm a b";
        equal_int "what Lean still reports" 2 (summary report "remaining");
        check "the recorded report" (Json.equal (without_account report) (without_account (Json.parse_exn (read "fixtures/mend/repair-rules.json"))));
        (* Without the table the name stays unknown; Lean's own advice is still taken. *)
        let bare = json_of (run_cli [ "mend"; "repair"; upgrade; "--no-ai"; "--no-prove"; "--json" ]) in
        equal_int "two repairs by rules" 2 (summary bare "by_rule");
        equal_int "three breakages remain" 3 (summary bare "remaining");
        let odd = run_cli [ "mend"; "repair"; upgrade; "--renames"; "PROJECT.toml" ] in
        equal_int "a table that is no table" (Exit_code.to_int Exit_code.Usage) odd.Proc.status;
        contains "its code" odd.Proc.stderr "MTH-MEND-0001");
    case "P17-03-02 a model's repairs are compiled, the smallest change is kept, and the file compiles again" (fun () ->
        let out = run_cli [ "mend"; "repair"; upgrade; "--renames"; renames; "--json" ] in
        equal_int "everything is repaired" 0 out.Proc.status;
        let report = json_of out in
        conforms "schemas/mend/report.schema.json" report;
        check "the recorded report" (Json.equal (without_account report) (without_account (Json.parse_exn (read "fixtures/mend/repair.json"))));
        check "three by a rule and two by a model"
          (stages report = [ ("sum_comm", "rule"); ("twice_add_zero", "rule"); ("le_succ_self", "rule"); ("twice_add", "model"); ("add_zero_eq", "model") ]);
        check "Lean accepts the whole file" (Json.member "clean" (Option.get (Json.member "summary" report)) = Some (Json.Bool true));
        let model = List.nth (Json.list_or_empty "repairs" report) 3 in
        (* The model offered a new proof and the old one with a step changed; both compile, and the smaller change is kept. *)
        equal_int "both proofs were compiled" 2 (Json.int_or 0 "tried" model);
        equal_int "one line out, one line in" 2 (Json.int_or 0 "changed_lines" model);
        contains "the proof keeps its shape" (Json.string_or "" "after" model) "| succ k ih =>\n    simp [twice] at *\n    omega";
        equal_int "two calls, served from cassettes" 2 (Json.int_or 0 "calls" (Option.get (Json.member "gateway" report)));
        check "the file was not touched" (Json.member "written" report = Some (Json.Bool false));
        (* The same with every provider, and written back: one line changes for each breakage. *)
        List.iter
          (fun provider ->
            let dir, copy = copy_of upgrade in
            let written = run_cli [ "mend"; "repair"; copy; "--renames"; renames; "--provider"; provider; "--write" ] in
            equal_int (provider ^ ": repaired") 0 written.Proc.status;
            contains (provider ^ ": the sum") written.Proc.stdout "5 breakage(s), 5 repaired (3 by a rule, 2 by a model, 0 by the prover), 0 left; Lean accepts the whole file";
            let after = is_ok "read" (Fs.read copy) in
            equal_int (provider ^ ": five lines differ") 10 (Mend_edit.changed_lines (read upgrade) after);
            equal_int (provider ^ ": nothing is broken any more") 0 (run_cli [ "mend"; "scan"; copy ]).Proc.status;
            equal_int (provider ^ ": and every theorem is verified") 0 (run_cli [ "check"; copy ]).Proc.status;
            Fs.remove_tree dir)
          [ "anthropic"; "openai"; "xai" ]);
    case "P17-03-04 without a model the prover proves again what its automation can, and the rest is left" (fun () ->
        let out = run_cli [ "mend"; "repair"; upgrade; "--renames"; renames; "--no-ai"; "--json" ] in
        equal_int "one proof is left" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let report = json_of out in
        conforms "schemas/mend/report.schema.json" report;
        check "the recorded report" (Json.equal (without_account report) (without_account (Json.parse_exn (read "fixtures/mend/repair-no-ai.json"))));
        check "the prover's proof for one, nothing for the other"
          (stages report = [ ("sum_comm", "rule"); ("twice_add_zero", "rule"); ("le_succ_self", "rule"); ("twice_add", "none"); ("add_zero_eq", "prove") ]);
        let proved = List.nth (Json.list_or_empty "repairs" report) 4 in
        equal_string "who found the proof" "portfolio" (Json.string_or "" "proposer" proved);
        let left = List.nth (Json.list_or_empty "repairs" report) 3 in
        contains "why the other was left" (Json.string_or "" "reason" left) "the prover did not prove the statement either";
        equal_int "no model was called" 0 (Json.int_or (-1) "calls" (Option.get (Json.member "gateway" report)));
        let text = run_cli [ "mend"; "repair"; upgrade; "--renames"; renames; "--no-ai" ] in
        contains "in text" text.Proc.stdout "add_zero_eq     type-mismatch       repaired  by the prover (portfolio)";
        contains "what is left" text.Proc.stdout "twice_add       unsolved-goals      left";
        contains "the file is not touched without --write" text.Proc.stdout "the file was not changed; --write writes the repairs back");
    case "P17-03-03 on Lean, a repaired theorem states what it stated, and nothing but a proof gets in" (fun () ->
        with_bridge (fun bridge ->
            let source = Mend_repair.sanitize (read upgrade) ~except:"twice_add" [ "sum_comm"; "twice_add"; "add_zero_eq" ] in
            let decl = is_ok "find" (Lean_source.find source "twice_add") in
            let on_lean = Mend_tools.theorem_check bridge decl in
            let reference = is_ok "reference" (Mend_agent.reference ~check:on_lean decl) in
            equal_string "what the theorem states, as Lean prints it" "\xE2\x88\x80 (m n : Nat), twice (m + n) = twice m + twice n" reference.Mend_check.statement;
            let ask proofs ~round:_ ~failures:_ = Ok proofs in
            (match Mend_agent.repair ~ask:(ask [ "by\n  simp [twice]\n  omega" ]) ~check:on_lean Mend_agent.default_settings decl with
             | Mend_agent.Fixed { proof; axioms; _ } ->
               equal_string "the proof Lean accepted" "by\n  simp [twice]\n  omega" proof;
               check "with the axioms it rests on" (List.mem "propext" axioms)
             | Mend_agent.Unfixed { reason; _ } -> fail reason);
            (* A text that ends the proof and states something else never reaches Lean. *)
            (match Mend_agent.repair ~ask:(ask [ "by simp [twice]; omega\ntheorem twice_add' : True := trivial" ]) ~check:on_lean { Mend_agent.rounds = 1; candidates = 1 } decl with
             | Mend_agent.Unfixed { attempts = [ attempt ]; _ } -> contains "stopped by the guard" attempt.Mend_agent.said "refused by the guard"
             | _ -> fail "a second theorem in a proof should be refused");
            (* A proof that Lean rejects comes back with Lean's words, for the next round. *)
            (match Mend_agent.repair ~ask:(ask [ "by rfl" ]) ~check:on_lean { Mend_agent.rounds = 1; candidates = 1 } decl with
             | Mend_agent.Unfixed { attempts = [ attempt ]; _ } -> check "Lean's error" (attempt.Mend_agent.said <> "" && not attempt.Mend_agent.accepted)
             | _ -> fail "rfl does not prove it")));
    case "P17-04-04 mend minimize shortens the proofs of a file, each shorter proof compiled by Lean" (fun () ->
        let out = run_cli [ "mend"; "minimize"; verbose; "--automation"; "--json" ] in
        equal_int "exit" 0 out.Proc.status;
        let result = json_of out in
        conforms "schemas/mend/minimization.schema.json" result;
        check "the recorded result" (Json.equal result (Json.parse_exn (read "fixtures/mend/minimization.json")));
        let results = Json.list_or_empty "results" result in
        equal_strings "how each proof fared" [ "deletion"; "deletion"; "deletion"; "automation"; "unchanged"; "unchanged" ] (List.map (Json.string_or "" "how") results);
        equal_string "a fact nobody used and a lemma simp never needed are gone" "by\n  unfold twice\n  simp only [Nat.two_mul]" (Json.string_or "" "after" (List.hd results));
        equal_string "one call for a whole proof" "by omega" (Json.string_or "" "after" (List.nth results 3));
        check "fewer lines in all" (summary result "lines_after" < summary result "lines_before");
        (* Without --automation whole proofs are not replaced. *)
        let plain = json_of (run_cli [ "mend"; "minimize"; verbose; "--json" ]) in
        equal_strings "deletion only" [ "deletion"; "deletion"; "deletion"; "unchanged"; "unchanged"; "unchanged" ] (List.map (Json.string_or "" "how") (Json.list_or_empty "results" plain));
        (* Written back, the file compiles, and there is nothing more to take out. *)
        let dir, copy = copy_of verbose in
        let written = run_cli [ "mend"; "minimize"; copy; "--write" ] in
        contains "the sum" written.Proc.stdout "3 of 6 proof(s) shortened, 23 line(s) to 17, every shorter proof checked by Lean";
        equal_int "the shorter file is verified" 0 (run_cli [ "check"; copy ]).Proc.status;
        contains "a second run finds nothing" (run_cli [ "mend"; "minimize"; copy ]).Proc.stdout "0 of 6 proof(s) shortened";
        Fs.remove_tree dir;
        let one = json_of (run_cli [ "mend"; "minimize"; verbose; "--theorem"; "twice_mono"; "--json" ]) in
        equal_int "one theorem by name" 1 (summary one "theorems");
        let absent = run_cli [ "mend"; "minimize"; verbose; "--theorem"; "no_such_theorem" ] in
        equal_int "a theorem that is not there" (Exit_code.to_int Exit_code.Usage) absent.Proc.status;
        contains "its code" absent.Proc.stderr "MTH-MEND-0003");
    case "P17-04-05 on Lean, a shorter proof that needs new axioms is refused, and the axioms are named" (fun () ->
        let result = json_of (run_cli [ "mend"; "minimize"; verbose; "--automation"; "--theorem"; "sum_le_plain"; "--theorem"; "sum_le"; "--json" ]) in
        (match Json.list_or_empty "results" result with
         | [ plain; automated ] ->
           equal_string "the proof without axioms stays" "unchanged" (Json.string_or "" "how" plain);
           check "it rests on none" (Json.list_or_empty "axioms" plain = []);
           contains "and the result says what the shorter proof would cost" (Json.string_or "" "note" plain) "it needs what this proof does without";
           contains "by name" (Json.string_or "" "note" plain) "propext";
           equal_string "the proof that already rests on them is replaced" "automation" (Json.string_or "" "how" automated);
           check "on the same axioms" (List.mem (Json.String "propext") (Json.list_or_empty "axioms" automated))
         | _ -> fail "two results were expected")) ]

(* ---- formalize ---- *)

let formalize_tests =
  let open Maithic_formalize in
  let problems name = "fixtures/formalize/problems/" ^ name in
  let recorded name = Json.parse_exn (read ("fixtures/formalize/" ^ name ^ ".json")) in
  (* A run without what differs from one run to the next: the account of the gateway, and where the file was written. *)
  let bare = function Json.Obj members -> Json.Obj (List.filter (fun (k, _) -> k <> "gateway" && k <> "written") members) | other -> other in
  let candidates j = Json.list_or_empty "candidates" j in
  let verdicts j = List.map (Json.string_or "" "verdict") (candidates j) in
  let check_of c name =
    match List.find_opt (fun k -> Json.string_or "" "name" k = name) (Json.list_or_empty "checks" c) with
    | Some k -> Json.string_or "" "outcome" k ^ (match Json.string_or "" "note" k with "" -> "" | note -> ": " ^ note)
    | None -> "no such check"
  in
  let given ?(id = 1) text = { Formalize_candidate.id; name = Printf.sprintf "problem_%d" id; signature = text; imports = []; reading = ""; provenance = Formalize_candidate.given } in
  let session bridge = match Formalize_tools.open_session bridge ~imports:[] with Ok s -> s | Error (`Imports why | `Commands why) -> fail why in
  [ case "P02-01-01 formalize takes the problem from an argument, from a file or from standard input" (fun () ->
        let statement = [ "--no-ai"; "--statement"; "(n : Int) (h : n < 0) : 0 < n ^ 2"; "--json" ] in
        let problem = read (problems "negative-square.txt") in
        let from_argument = json_of (run_cli ([ "formalize"; String.trim problem ] @ statement)) in
        let from_file = json_of (run_cli ([ "formalize"; "--file"; problems "negative-square.txt" ] @ statement)) in
        let from_stdin = json_of (run_cli ~stdin:problem ([ "formalize"; "--file"; "-" ] @ statement)) in
        let from_pipe = json_of (run_cli ~stdin:problem ([ "formalize" ] @ statement)) in
        let origin j = Json.string_or "" "origin" (Option.get (Json.member "problem" j)) and text j = Json.string_or "" "text" (Option.get (Json.member "problem" j)) in
        equal_strings "where each came from" [ "argument"; "fixtures/formalize/problems/negative-square.txt"; "stdin"; "stdin" ]
          (List.map origin [ from_argument; from_file; from_stdin; from_pipe ]);
        List.iter (fun j -> equal_string "the same problem" "Show that the square of a negative integer is positive." (text j)) [ from_argument; from_file; from_stdin; from_pipe ];
        List.iter (fun j -> conforms "schemas/formalize/run.schema.json" j) [ from_argument; from_file; from_stdin; from_pipe ];
        let twice = run_cli ([ "formalize"; "A problem."; "--file"; problems "negative-square.txt" ] @ statement) in
        equal_int "a problem given twice" (Exit_code.to_int Exit_code.Usage) twice.Proc.status;
        contains "its code" (twice.Proc.stdout ^ twice.Proc.stderr) "MTH-FORMALIZE-0001";
        let missing = run_cli ([ "formalize"; "--file"; "no/such/problem.txt" ] @ statement) in
        equal_int "a file that is not there" (Exit_code.to_int Exit_code.Usage) missing.Proc.status;
        let nothing_to_draft = run_cli [ "formalize"; "A problem."; "--no-ai" ] in
        equal_int "no model and no statement" (Exit_code.to_int Exit_code.Usage) nothing_to_draft.Proc.status;
        contains "says what to give" nothing_to_draft.Proc.stderr "give the statements to check with --statement";
        equal_int "a name Lean would not take" (Exit_code.to_int Exit_code.Usage) (run_cli ([ "formalize"; "A problem."; "--name"; "2fast" ] @ statement)).Proc.status);
    case "P02-02-01 formalize has statements drafted for a problem, and chooses one that holds up" (fun () ->
        let out = run_cli [ "formalize"; "--file"; problems "negative-square.txt"; "--candidates"; "4"; "--json" ] in
        equal_int "a statement that no check flagged" 0 out.Proc.status;
        let run = json_of out in
        conforms "schemas/formalize/run.schema.json" run;
        check "the run the repository keeps" (Json.equal (bare run) (bare (recorded "negative-square")));
        equal_strings "four statements, and what became of each" [ "chosen"; "rejected"; "rejected"; "duplicate" ] (verdicts run);
        let account = Option.get (Json.member "gateway" run) in
        equal_int "four calls, served from cassettes: a draft, a repair, a reading and a comparison" 4 (Json.int_or 0 "calls" account);
        equal_strings "in this order" [ "formalize.draft"; "formalize.repair"; "formalize.back"; "formalize.judge" ]
          (List.map (Json.string_or "" "purpose") (Json.list_or_empty "records" account));
        (* The same with every provider, written as a file that Lean reads and the prover proves. *)
        List.iter
          (fun provider ->
            let dir = Fs.temp_dir "maithic-formalize" in
            let file = Fs.(dir / "NegativeSquare.lean") in
            let written = run_cli [ "formalize"; "--file"; problems "negative-square.txt"; "--candidates"; "4"; "--provider"; provider; "--write"; file ] in
            equal_int (provider ^ ": formalized") 0 written.Proc.status;
            contains (provider ^ ": in a line") written.Proc.stdout "formalized: candidate 1 of 4 (1 repaired, 1 duplicate, 2 rejected)";
            contains (provider ^ ": the statement") written.Proc.stdout "theorem problem (n : Int) (h : n < 0) : 0 < n ^ 2";
            equal_string (provider ^ ": the file the repository keeps") (read "fixtures/formalize/lean/NegativeSquare.lean") (is_ok "read" (Fs.read file));
            equal_int (provider ^ ": Lean reads it, and it is not proved") (Exit_code.to_int Exit_code.Not_verified) (run_cli [ "check"; file ]).Proc.status;
            let proved = run_cli [ "prove"; file; "--provider"; provider ] in
            equal_int (provider ^ ": the prover proves it") 0 proved.Proc.status;
            contains (provider ^ ": verified") proved.Proc.stdout "problem  verified  (model";
            Fs.remove_tree dir)
          [ "anthropic"; "openai"; "xai" ];
        (* A problem nobody authored answers for cannot be drafted in replay mode, and the command says so. *)
        let unanswered = run_cli [ "formalize"; "Show that every prime above 2 is odd." ] in
        equal_int "no answer" (Exit_code.to_int Exit_code.Provider) unanswered.Proc.status;
        contains "its code" unanswered.Proc.stderr "MTH-FORMALIZE-0004");
    case "P02-03-04 formalize repairs a statement round by round, each time with what Lean said, and gives up at the bound" (fun () ->
        let out = run_cli [ "formalize"; "--file"; problems "square-bound.txt"; "--candidates"; "4"; "--json" ] in
        equal_int "a statement is chosen" 0 out.Proc.status;
        let run = json_of out in
        conforms "schemas/formalize/run.schema.json" run;
        check "the run the repository keeps" (Json.equal (bare run) (bare (recorded "square-bound")));
        equal_strings "what became of each" [ "chosen"; "duplicate"; "kept"; "failed" ] (verdicts run);
        let history c = List.map (fun a -> (Json.string_or "" "statement" a, Json.string_or "" "error_class" a)) (Json.list_or_empty "history" (Option.get (Json.member "elaboration" c))) in
        check "two repairs, each for what Lean reported"
          (history (List.nth (candidates run) 1)
           = [ ("(n : \xE2\x84\x95) : n \xE2\x89\xA4 n\xC2\xB2", "syntax"); ("(n : \xE2\x84\x95) : n \xE2\x89\xA4 n ^ 2", "unknown-constant"); ("(n : Nat) : n \xE2\x89\xA4 n ^ 2", "") ]);
        equal_string "and then Lean shows it says what the first says" "definitional"
          (Json.string_or "" "relation" (Option.get (Json.member "duplicate_of" (List.nth (candidates run) 1))));
        let last = List.nth (candidates run) 3 in
        equal_int "three statements were compiled for the last" 3 (List.length (history last));
        equal_string "and it was given up" "it does not elaborate after 2 repair round(s)" (Json.string_or "" "reason" last);
        (* With no round, nothing is repaired; the model is asked for the draft and the readings only. *)
        let unrepaired = json_of (run_cli [ "formalize"; "--file"; problems "square-bound.txt"; "--candidates"; "4"; "--rounds"; "0"; "--json" ]) in
        equal_strings "without repair" [ "chosen"; "failed"; "kept"; "failed" ] (verdicts unrepaired);
        equal_int "a draft and two readings with their comparisons" 5 (Json.int_or 0 "calls" (Option.get (Json.member "gateway" unrepaired))));
    case "P02-04-02 formalize reads each statement back, and flags the one that differs from the problem" (fun () ->
        let out = run_cli [ "formalize"; "--file"; problems "subtraction.md"; "--json" ] in
        equal_int "the chosen statement is flagged" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let run = json_of out in
        conforms "schemas/formalize/run.schema.json" run;
        check "the run the repository keeps" (Json.equal (bare run) (bare (recorded "subtraction")));
        equal_string "the status" "flagged" (Json.string_or "" "status" run);
        equal_strings "what became of each" [ "rejected"; "chosen"; "kept" ] (verdicts run);
        let chosen = List.nth (candidates run) 1 in
        equal_int "the agreement a model gave" 7 (Json.int_or 0 "agreement" chosen);
        equal_strings "the difference it named" [ "The statement assumes that b is at most a; the problem makes no such assumption." ]
          (Json.strings (Json.List (Json.list_or_empty "differences" chosen)));
        equal_string "the round trip" "flagged: agreement 7 of 10: The statement assumes that b is at most a; the problem makes no such assumption." (check_of chosen "round-trip");
        contains "what the statement says, in English" (Json.string_or "" "english" chosen) "subtracting b from a and then adding b gives a";
        check "the statement Lean refuted was never read back" (Json.member "english" (List.hd (candidates run)) = None);
        let text = run_cli [ "formalize"; "--file"; problems "subtraction.md" ] in
        contains "the problem, with its marks taken off" text.Proc.stdout "For natural numbers a and b, show that (a - b) + b = a.";
        contains "the refutation" text.Proc.stdout "counterexample    failed   it fails at a = 0, b = 1";
        contains "in a line" text.Proc.stdout "chosen with a flag: candidate 2 of 3, flagged by traps and round-trip (1 rejected)");
    case "P02-04-05 formalize sends each statement to Refute, and one that Lean refutes or shows to be empty is discarded" (fun () ->
        let out =
          run_cli
            [ "formalize"; "--file"; problems "subtraction.md"; "--no-ai"; "--statement"; "(a b : Nat) : a - b + b = a"; "--statement"; "(a b : Nat) (h : b \xE2\x89\xA4 a) : a - b + b = a"; "--json" ]
        in
        equal_int "flagged" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let run = json_of out in
        conforms "schemas/formalize/run.schema.json" run;
        check "the run the repository keeps" (Json.equal (bare run) (recorded "subtraction-no-ai"));
        check "no model was asked" (Json.member "gateway" run = None);
        equal_string "the literal statement is false" "refuted: it fails at a = 0, b = 1" (Json.string_or "" "reason" (List.hd (candidates run)));
        let second = List.nth (candidates run) 1 in
        equal_string "the other survives the test" "passed: no counterexample in 300 sampled cases" (check_of second "counterexample");
        equal_string "its trap is named" "flagged: a - b: subtraction of natural numbers stops at zero: it is 0 whenever the second number is the larger" (check_of second "traps");
        equal_string "nothing was read back" "skipped: no model was asked" (check_of second "back-translation");
        (* Hypotheses that cannot hold together, a statement simp closes, and a variable nothing mentions. *)
        let more =
          json_of
            (run_cli
               [ "formalize"; "Show that the square of a negative integer is positive."; "--no-ai"; "--statement"; "(n : Nat) (h : n < 0) : 0 < n ^ 2"; "--statement";
                 "(n : Int) (h : n * 0 = 0) : n * 0 = 0"; "--statement"; "(n m : Int) (h : n < 0) : 0 < n ^ 2"; "--statement"; "(n : Int) (h : n < 0) : 0 < n * n * n * n"; "--json" ])
        in
        equal_strings "what became of each" [ "rejected"; "kept"; "kept"; "chosen" ] (verdicts more);
        equal_string "the empty statement" "vacuous: the hypothesis h cannot hold (shown by contradiction)" (Json.string_or "" "reason" (List.hd (candidates more)));
        contains "the trivial one" (check_of (List.nth (candidates more) 1) "triviality") "flagged: Lean accepts `by ";
        equal_string "the idle variable" "flagged: nothing in the statement mentions m" (check_of (List.nth (candidates more) 2) "variables"));
    case "P02-03-01 on Lean, a candidate is compiled where only the allowed modules are known, and its text passes the guard" (fun () ->
        with_bridge (fun bridge ->
            let s = session bridge in
            let compile = Formalize_tools.compile s in
            let good = compile (given "(n : Int) (h : n < 0) : 0 < n ^ 2") in
            check "a statement of the core library elaborates" good.Formalize_elab.ok;
            equal_string "as Lean prints it" "\xE2\x88\x80 (n : Int), n < 0 \xE2\x86\x92 0 < n ^ 2" good.Formalize_elab.statement;
            let unknown = compile (given "(a : Nat) (h : Even a) : Even (a + a)") in
            check "a name the core library does not have" (not unknown.Formalize_elab.ok);
            contains "what Lean said" unknown.Formalize_elab.said "Unknown identifier `Even`";
            equal_string "its class" "unknown-constant" (Formalize_error.name (Formalize_error.classify unknown.Formalize_elab.said));
            let undeclared = compile (given "(h : n < 0) : 0 < n ^ 2") in
            check "a variable that is not declared is an error, not a guess" (not undeclared.Formalize_elab.ok);
            let inner = compile (given "(t : Maithic.Formalize.Trap) : t = t") in
            check "the commands of Maithic are not part of what a statement may use" (not inner.Formalize_elab.ok);
            let smuggled = compile (given "(n : Nat) : n = n := rfl\ntheorem extra : False := sorry\ntheorem more (n : Nat) : n = n") in
            check "a second theorem in a statement never reaches Lean" (not smuggled.Formalize_elab.ok);
            contains "stopped by the guard" smuggled.Formalize_elab.said "the guard of the Bridge refused the statement";
            (* The loop, with Lean in it: the repair is compiled like the draft. *)
            let repair ~candidate:_ ~error ~said:_ ~failures:_ =
              equal_string "the class the repair is asked for" "unknown-constant" (Formalize_error.name error);
              Ok ("(a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0", Formalize_candidate.given)
            in
            let fixed = Formalize_elab.run ~compile ~repair (given "(a : Nat) (h : Even a) : Even (a + a)") in
            check "repaired" fixed.Formalize_elab.elaborated;
            equal_string "the statement Lean accepted" "\xE2\x88\x80 (a : Nat), a % 2 = 0 \xE2\x86\x92 (a + a) % 2 = 0" fixed.Formalize_elab.statement));
    case "P02-03-03 on Lean, the names a statement uses are looked up for the repair" (fun () ->
        with_bridge (fun bridge ->
            let s = session bridge in
            let found = Formalize_tools.lookup s (Formalize_repair.identifiers "(a b : Nat) (ha : Even a) (hb : b = Nat.succ a) : Even (a + List.length [a, b])") in
            equal_strings "the names Lean knows" [ "Nat"; "Nat.succ"; "List.length" ] (List.map fst found);
            equal_string "a type" "Nat : Type" (List.assoc "Nat" found);
            equal_string "a function, with its arguments" "Nat.succ (n : Nat) : Nat" (List.assoc "Nat.succ" found);
            contains "a function over any type" (List.assoc "List.length" found) "List \xCE\xB1 \xE2\x86\x92 Nat";
            check "nothing for a name it does not know" (not (List.mem_assoc "Even" found));
            equal_int "and nothing for no name" 0 (List.length (Formalize_tools.lookup s []))));
    case "P02-04-03 on Lean, a statement is declared for the checks and each check names it" (fun () ->
        with_bridge (fun bridge ->
            let s = session bridge in
            let lean = (Formalize_tools.tools ~name:"problem" ~imports:[] ~intake:(is_ok "intake" (Formalize_intake.of_text "A problem.")) s).Formalize_run.lean in
            let empty = given "(n : Nat) (h : n < 0) : 0 < n ^ 2" and sound = given ~id:2 "(n : Int) (h : n < 0) : 0 < n ^ 2" in
            check "hypotheses that cannot hold" (is_ok "vacuous" (lean.Formalize_faith.vacuous empty) = Some "the hypothesis h cannot hold (shown by contradiction)");
            check "hypotheses that can" (is_ok "vacuous" (lean.Formalize_faith.vacuous sound) = None);
            (match is_ok "refute" (lean.Formalize_faith.refute (given ~id:3 "(n : Int) (h : n \xE2\x89\xA4 0) : 0 < n ^ 2")) with
             | Formalize_faith.Refuted point -> check "a counterexample Lean confirmed" (point = [ ("n", "0") ])
             | Formalize_faith.Survived _ -> fail "the statement is false at zero");
            (match is_ok "refute" (lean.Formalize_faith.refute sound) with
             | Formalize_faith.Survived tried -> equal_string "what was tried" "300 sampled cases" tried
             | Formalize_faith.Refuted _ -> fail "the statement is true");
            contains "a statement that cannot be sampled says so" (is_error "refute" (lean.Formalize_faith.refute (given ~id:4 "(a : Nat) (h : \xE2\x88\x83 k, a = 2 * k) : \xE2\x88\x83 k, a + a = 2 * k")))
              "cannot be tested by sampling";
            check "what trivial, rfl and simp cannot close" (lean.Formalize_faith.closes sound Formalize_faith.trivial_proofs = None);
            check "and what they can" (lean.Formalize_faith.closes (given ~id:5 "(n : Nat) : n + 0 = n") Formalize_faith.trivial_proofs <> None);
            let inspection = is_ok "inspect" (lean.Formalize_faith.inspect (given ~id:6 "(a b spare : Nat) (h : b \xE2\x89\xA4 a) : a - b + b = a")) in
            equal_strings "a variable nothing mentions" [ "spare" ] inspection.Formalize_faith.unused;
            equal_strings "an operation that is not what it seems" [ "nat-subtraction" ] (List.map (fun (kind, _, _) -> kind) inspection.Formalize_faith.traps);
            (* Two statements that say the same, and two that do not. *)
            let same = Formalize_tools.same s in
            check "the same once definitions are unfolded" (same sound (given ~id:7 "(n : Int) (h : n < 0) : n ^ 2 > 0") = Some "definitional");
            check "the same text" (same sound (given ~id:8 "(n : Int) (h : n < 0) : 0 < n ^ 2") = Some "identical");
            check "a weaker statement is another statement" (same sound (given ~id:9 "(n : Int) (h : n < -1) : 0 < n ^ 2") = None);
            check "and so is one that only a proof connects to it" (same sound (given ~id:10 "(n : Int) (h : n < 0) : 0 < n * n") = None))) ]

(* ---- council ---- *)

let council_tests =
  let open Maithic_council in
  (* A session without what differs from one run to the next: the moments, the durations and the account of the gateway. *)
  let rec timeless = function
    | Json.Obj members -> Json.Obj (List.filter_map (fun (k, v) -> if List.mem k [ "at"; "seconds"; "gateway" ] then None else Some (k, timeless v)) members)
    | Json.List items -> Json.List (List.map timeless items)
    | other -> other
  in
  let turns j = List.map (fun t -> Json.string_or "" "role" t ^ " " ^ Json.string_or "" "task" t ^ " " ^ Json.string_or "" "status" t) (Json.list_or_empty "turns" j) in
  let ending j = Json.string_or "" "reason" (Option.get (Json.member "ending" j)) in
  let spent j key = Json.int_or (-1) key (Option.get (Json.member "spent" (Option.get (Json.member "budget" j)))) in
  let problem_of file theorem = is_ok "problem" (Council_problem.of_source ~theorem ~file (read file)) in
  [ case "P06-01-03 council holds a session on a theorem, turn by turn, and Lean's kernel settles it" (fun () ->
        let out = run_cli [ "council"; "examples/council.lean"; "--json" ] in
        equal_int "the claim is verified" 0 out.Proc.status;
        let session = json_of out in
        conforms "schemas/council/session.schema.json" session;
        check "the session the repository keeps" (Json.equal (timeless session) (timeless (Json.parse_exn (read "fixtures/council/sum-odd.json"))));
        equal_strings "the turns that were taken"
          [ "verifier examine done"; "researcher research done"; "prover prove done"; "verifier verify done"; "critic critique done"; "prover prove done"; "verifier verify done" ]
          (turns session);
        equal_string "how it ended" "settled" (ending session);
        equal_int "four questions to models, served from cassettes" 4 (spent session "model_calls");
        let answer = Option.get (Json.member "answer" session) in
        equal_string "the answer" "verified" (Json.string_or "" "status" answer);
        equal_strings "the axioms the proof rests on" [ "Quot.sound"; "propext" ] (Json.strings (Json.List (Json.list_or_empty "axioms" answer)));
        equal_int "the answer is built from the claim, the proof and the three lemmas the proof uses" 5 (List.length (Json.list_or_empty "components" answer));
        (* The researcher named a lemma that does not exist, and it never reached the board. *)
        let board = Json.list_or_empty "board" session in
        equal_strings "the lemmas on the board are the ones Lean knows" [ "sumOdd"; "Nat.succ_mul"; "Nat.mul_succ"; "Nat.mul_comm" ]
          (List.filter_map (fun i -> if Json.string_or "" "kind" i = "definition" then Some (Json.string_or "" "title" i) else None) board);
        equal_strings "what Lean said about each proof" [ "rejected"; "rejected"; "accepted" ]
          (List.filter_map (fun i -> if Json.string_or "" "kind" i = "proof" then Some (Json.string_or "" "examined" i) else None) board);
        (* The same with every provider, written back and as a transcript. *)
        List.iter
          (fun provider ->
            let dir = Fs.temp_dir "maithic-council" in
            let copy = Fs.(dir / "council.lean") and transcript = Fs.(dir / "session.md") in
            ignore (is_ok "copy" (Fs.write copy (read "examples/council.lean")));
            let written = run_cli [ "council"; copy; "--provider"; provider; "--write"; "--transcript"; transcript ] in
            equal_int (provider ^ ": verified") 0 written.Proc.status;
            contains (provider ^ ": in a line") written.Proc.stdout "verified: proof 11, by the prover in round 2 (4 model calls, 24 calls to Lean)";
            equal_string (provider ^ ": the transcript the repository keeps") (read "fixtures/council/sum-odd.md") (is_ok "read" (Fs.read transcript));
            contains (provider ^ ": the proof is in the file") (is_ok "read" (Fs.read copy)) "theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by\n  induction n with";
            equal_int (provider ^ ": and Lean verifies the file") 0 (run_cli [ "check"; copy ]).Proc.status;
            Fs.remove_tree dir)
          [ "anthropic"; "openai"; "xai" ];
        let text = run_cli [ "council"; "examples/council.lean" ] in
        contains "a line for each turn" text.Proc.stdout "  researcher  research  4 of 5 names exist in the library; 1 idea";
        contains "the file is not touched without --write" text.Proc.stdout "the file was not changed; --write writes the proof into it");
    case "P06-01-01 council takes a theorem of a file, and says what the file holds when it cannot" (fun () ->
        let several = run_cli [ "council"; "examples/conjectures.lean" ] in
        equal_int "several unfinished theorems need a name" (Exit_code.to_int Exit_code.Usage) several.Proc.status;
        contains "its code" several.Proc.stderr "MTH-COUNCIL-0001";
        contains "the theorems there are" several.Proc.stderr "sum_le_product, square_monotone";
        equal_int "a theorem that is not there" (Exit_code.to_int Exit_code.Usage) (run_cli [ "council"; "examples/council.lean"; "--theorem"; "missing" ]).Proc.status;
        let dir = Fs.temp_dir "maithic-council" in
        let broken = Fs.(dir / "Broken.lean") in
        ignore (is_ok "write" (Fs.write broken "def f : Nat := \"no\"\n\ntheorem t : f = 1 := by\n  sorry\n"));
        let out = run_cli [ "council"; broken; "--no-ai" ] in
        equal_int "a file with errors before the theorem" (Exit_code.to_int Exit_code.Failure) out.Proc.status;
        contains "its code" out.Proc.stderr "MTH-COUNCIL-0002";
        Fs.remove_tree dir;
        let asked = json_of (run_cli [ "council"; "examples/even.lean"; "--no-ai"; "--goal"; "Is there a one-line proof?"; "--json" ]) in
        equal_string "what was asked in words is part of the problem" "Is there a one-line proof?" (Json.string_or "" "goal" (Option.get (Json.member "problem" asked)));
        check "and is on the board as a question" (List.exists (fun i -> Json.string_or "" "kind" i = "question") (Json.list_or_empty "board" asked)));
    case "P06-02-04 council refutes a false statement in the examination, before any model is asked" (fun () ->
        let out = run_cli [ "council"; "examples/conjectures.lean"; "--theorem"; "sum_le_product"; "--json" ] in
        equal_int "the exit status of a refutation" (Exit_code.to_int Exit_code.Refuted) out.Proc.status;
        let session = json_of out in
        conforms "schemas/council/session.schema.json" session;
        equal_string "refuted" "refuted" (Json.string_or "" "status" session);
        equal_strings "one turn" [ "verifier examine done" ] (turns session);
        equal_int "no model was asked" 0 (spent session "model_calls");
        let answer = Option.get (Json.member "answer" session) in
        equal_string "the point where it fails" "a := 1\nb := 0" (Json.string_or "" "counterexample" answer);
        let claim = List.nth (Json.list_or_empty "board" session) 1 in
        equal_strings "the status rests on a refutation Lean accepted" [ "elaborated"; "disproved" ] (List.map (Json.string_or "" "kind") (Json.list_or_empty "evidence" claim)));
    case "P06-05-02 the prove tool is the prover's automation, and it settles what automation can" (fun () ->
        let out = run_cli [ "council"; "examples/even.lean"; "--no-ai"; "--json" ] in
        equal_int "verified without a model" 0 out.Proc.status;
        let session = json_of out in
        conforms "schemas/council/session.schema.json" session;
        equal_strings "in the examination" [ "verifier examine done" ] (turns session);
        let proof = List.find (fun i -> Json.string_or "" "kind" i = "proof") (Json.list_or_empty "board" session) in
        equal_string "the proof is posted by the verifier, with where it came from" "verifier: from the prover's automation (portfolio)" (Json.string_or "" "author" proof ^ ": " ^ Json.string_or "" "title" proof);
        equal_string "and Lean accepted it as it accepts any proof" "accepted" (Json.string_or "" "examined" proof);
        check "the settings say that no model was asked" (Json.member "model" (Option.get (Json.member "settings" session)) = Some (Json.Bool false));
        (* What automation cannot prove stays unproved without a model, and the command says so. *)
        let alone = run_cli [ "council"; "examples/council.lean"; "--no-ai" ] in
        equal_int "not proved" (Exit_code.to_int Exit_code.Not_verified) alone.Proc.status;
        contains "in a line" alone.Proc.stdout "not proved: the claim is formalized; no model was asked, so no round followed the examination");
    case "P06-06-01 a budget ends a session between two calls, and what was established stays" (fun () ->
        let out = run_cli [ "council"; "examples/council.lean"; "--budget-model-calls"; "2"; "--json" ] in
        equal_int "not proved" (Exit_code.to_int Exit_code.Not_verified) out.Proc.status;
        let session = json_of out in
        conforms "schemas/council/session.schema.json" session;
        equal_string "ended by the budget" "budget" (ending session);
        equal_string "the limit that was reached" "the limit of 2 calls to a model is reached" (Json.string_or "" "detail" (Option.get (Json.member "ending" session)));
        equal_strings "the proofs of the last call were still given to Lean" [ "verifier examine done"; "researcher research done"; "prover prove done"; "verifier verify done" ] (turns session);
        equal_string "the claim keeps the status it had reached" "formalized" (Json.string_or "" "status" session);
        check "the answer holds no proof" (Json.member "proof" (Option.get (Json.member "answer" session)) = None);
        let priced = json_of (run_cli [ "council"; "examples/even.lean"; "--no-ai"; "--budget-cost"; "1.5"; "--price-input"; "3"; "--price-output"; "15"; "--json" ]) in
        let budget = Option.get (Json.member "budget" priced) in
        check "a limit on cost, with the prices it is counted at" (Json.member "prices" budget <> None && Json.member "cost" (Option.get (Json.member "limits" budget)) = Some (Json.Float 1.5));
        equal_int "a limit must be a number" (Exit_code.to_int Exit_code.Usage) (run_cli [ "council"; "examples/even.lean"; "--budget-lean-calls"; "many" ]).Proc.status);
    case "P06-05-01 on Lean, the check tool returns diagnostics, and what a model wrote passes the guard first" (fun () ->
        with_bridge (fun bridge ->
            let problem = problem_of "examples/council.lean" "sumOdd_eq_square" in
            let policy = { Trust_policy.default with Trust_policy.require_recheck = false } in
            let tools = Council_tools.make (is_ok "raw" (Council_real.raw ~policy bridge problem)) in
            let known = is_ok "check" (Council_tools.lean_check tools "#check sumOdd") in
            equal_strings "a definition of the file is known where the theorem stands" [ "info: sumOdd : Nat \xE2\x86\x92 Nat" ]
              (List.map (fun (d : Council_tools.diagnostic) -> d.Council_tools.severity ^ ": " ^ d.Council_tools.text) known);
            let unknown = is_ok "check" (Council_tools.lean_check tools ~untrusted:"Nat.add_mul_self_eq" "#check Nat.add_mul_self_eq") in
            (match Council_tools.errors unknown with
             | [ error ] -> contains "Lean's own words" error "Nat.add_mul_self_eq"
             | _ -> fail "one error was expected");
            contains "a name that is more than a name never reaches Lean"
              (is_error "guard" (Council_tools.lean_check tools ~untrusted:"Nat\ntheorem extra : False := by simp" "#check Nat\ntheorem extra : False := by simp"))
              "the guard of the Bridge refused the text";
            equal_strings "the names Lean knows, of those a model gave" [ "sumOdd"; "Nat.succ_mul" ] (List.map fst (Council_tools.lookup tools [ "sumOdd"; "Nat.add_mul_self_eq"; "Nat.succ_mul" ]));
            let formal = Council_evidence.statement (is_ok "elaborate" (Council_tools.elaborate tools)) in
            equal_string "the statement as Lean prints it" "\xE2\x88\x80 (n : Nat), sumOdd n = n * n" formal;
            let good = "by\n  induction n with\n  | zero => rfl\n  | succ k ih =>\n    simp only [sumOdd, ih, Nat.succ_mul, Nat.mul_succ]\n    omega" in
            (match Council_tools.check_proof tools ~policy ~statement:formal good with
             | Ok evidence -> equal_strings "a proof Lean accepts is evidence, with its axioms" [ "Quot.sound"; "propext" ] (Council_evidence.axioms evidence)
             | Error _ -> fail "the proof should be accepted");
            (match Council_tools.check_proof tools ~policy ~statement:formal "by omega" with
             | Error (Council_tools.Rejected why) -> contains "a proof Lean rejects comes back with what Lean said" why "omega could not prove the goal"
             | _ -> fail "omega does not prove it");
            (match Council_tools.check_proof tools ~policy ~statement:formal "by sorry" with
             | Error (Council_tools.Rejected why) -> contains "sorry is stopped by the guard" why "the guard of the Bridge refused the text"
             | _ -> fail "sorry is no proof");
            (match Council_tools.check_proof tools ~policy ~statement:formal "by\n  induction n <;> simp [sumOdd, Nat.succ_mul, Nat.mul_succ] <;> omega\ntheorem extra : 1 = 1 := rfl" with
             | Error (Council_tools.Rejected why) -> contains "and so is a second theorem behind a proof" why "the guard of the Bridge refused the text"
             | _ -> fail "a second theorem should be refused");
            (* Under the default policy a proof counts only after a second check in a fresh process; without one it does not. *)
            let strict = Council_tools.make (is_ok "raw" (Council_real.raw ~policy:Trust_policy.default bridge problem)) in
            (match Council_tools.check_proof strict ~policy:Trust_policy.default ~statement:formal good with
             | Error (Council_tools.Rejected why) -> contains "no second check, no evidence" why "the trust policy is not met"
             | _ -> fail "a proof that was not checked again should not count under the default policy");
            let recheck ~source name = Recheck.fresh ?cwd:located.Bridge_locate.cwd ~argv:located.Bridge_locate.argv ~source name in
            let rechecked = Council_tools.make (is_ok "raw" (Council_real.raw ~recheck ~policy:Trust_policy.default bridge problem)) in
            check "with one, it counts" (Result.is_ok (Council_tools.check_proof rechecked ~policy:Trust_policy.default ~statement:formal good)))) ]

(* ---- blueprints ---- *)

let blueprint_tests =
  let blueprint = "examples/blueprint/triangles.md" in
  let snapshot name = Json.parse_exn (read ("fixtures/blueprint/snapshots/" ^ name ^ ".json")) in
  let lines (out : Proc.output) = List.filter (fun l -> l <> "") (String.split_on_char '\n' out.Proc.stdout) in
  [ case "P16-01-06 blueprint check accepts a sound blueprint and reports a circle" (fun () ->
        let out = run_cli [ "blueprint"; "check"; blueprint ] in
        equal_int "sound" 0 out.Proc.status;
        contains "what it says" out.Proc.stdout "examples/blueprint/triangles.md: 8 node(s), 13 edge(s), no problem";
        let dir = temp_dir "blueprint-check" in
        let circle = Fs.(dir / "circle.md") in
        is_ok "written" (Fs.write circle "---\nblueprint: 1\ntitle: A circle\n---\n\n## Lemma `a`: A\n\n- uses: `b`\n\nA.\n\n## Lemma `b`: B\n\n- proof uses: `a`\n\nB.\n");
        let out = run_cli [ "blueprint"; "check"; circle ] in
        equal_int "a problem" 7 out.Proc.status;
        contains "the circle" out.Proc.stdout "the nodes a -> b -> a depend on each other in a circle";
        let out = run_cli [ "blueprint"; "skeleton"; circle; "--no-check" ] in
        equal_int "nothing is built on it" 2 out.Proc.status;
        contains "the code" (out.Proc.stdout ^ out.Proc.stderr) "MTH-BLUEPRINT-0002";
        let out = run_cli [ "blueprint"; "check"; "examples/even.lean" ] in
        equal_int "not a blueprint" 2 out.Proc.status;
        contains "the code" (out.Proc.stdout ^ out.Proc.stderr) "MTH-BLUEPRINT-0001";
        Fs.remove_tree dir);
    case "P16-01-07 blueprint graph exports the graph as JSON and in the DOT language" (fun () ->
        let out = run_cli [ "blueprint"; "graph"; blueprint ] in
        equal_int "written" 0 out.Proc.status;
        let graph = json_of out in
        conforms "schemas/blueprint/graph.schema.json" graph;
        check "the graph the repository keeps" (Json.equal graph (Json.parse_exn (read "fixtures/blueprint/graph.json")));
        let dot = run_cli [ "blueprint"; "graph"; blueprint; "--format"; "dot" ] in
        contains "DOT" dot.Proc.stdout "digraph blueprint {";
        contains "an edge of a proof" dot.Proc.stdout "\"odd-triangles\" -> \"two-triangles\" [style=dashed];";
        equal_int "an unknown format" 2 (run_cli [ "blueprint"; "graph"; blueprint; "--format"; "png" ]).Proc.status);
    case "P16-02-02 blueprint plan writes the plan of an argument as a blueprint, from the cassettes" (fun () ->
        let dir = temp_dir "blueprint-plan" in
        let out_file = Fs.(dir / "plan.md") in
        let out = run_cli [ "blueprint"; "plan"; "examples/blueprint/argument.md"; "--namespace"; "Triangles"; "--out"; out_file ] in
        equal_int "a plan" 0 out.Proc.status;
        same_file "the plan the repository keeps" "fixtures/blueprint/plan.blueprint.md" out_file;
        contains "an identifier was made plain" out.Proc.stdout "note: the identifier 'Odd Square' became 'odd-square'";
        contains "a reference to nothing was dropped" out.Proc.stdout "note: 'tri-formula' referred to 'gauss', which is not a node";
        contains "the summary" out.Proc.stdout "8 node(s), 13 edge(s), 8 stated in Lean";
        equal_int "the plan is a sound blueprint" 0 (run_cli [ "blueprint"; "check"; out_file ]).Proc.status;
        (* the nodes of the example are the nodes of this plan *)
        let nodes text = match Text.find_sub text "\n## " with Some i -> String.sub text i (String.length text - i) | None -> "" in
        equal_string "the example is the plan with an introduction" (nodes (read "fixtures/blueprint/plan.blueprint.md")) (nodes (read blueprint));
        let off = run_cli ~env:(("MAITHIC_GATEWAY_MODE", "off") :: List.remove_assoc "MAITHIC_GATEWAY_MODE" clean_env) [ "blueprint"; "plan"; "examples/blueprint/argument.md"; "--out"; out_file ] in
        check "without a model there is no plan" (off.Proc.status <> 0);
        Fs.remove_tree dir);
    case "P16-03-02 blueprint skeleton writes a skeleton that Lean compiles, and lays an error at the door of its node" (fun () ->
        let dir = temp_dir "blueprint-skeleton" in
        let lean = Fs.(dir / "skeleton.lean") in
        let out = run_cli [ "blueprint"; "skeleton"; blueprint; "--out"; lean; "--json" ] in
        equal_int "it compiles" 0 out.Proc.status;
        let j = json_of out in
        check "Lean says so" (Json.bool_or false "compiles" j);
        equal_int "eight declarations" 8 (Json.int_or 0 "stated" j);
        same_file "the skeleton the repository keeps" "fixtures/blueprint/lean/Stage1.lean" lean;
        let again = run_cli [ "blueprint"; "skeleton"; blueprint; "--out"; lean ] in
        equal_int "a file that exists is not written over" 2 again.Proc.status;
        equal_int "unless asked" 0 (run_cli [ "blueprint"; "skeleton"; blueprint; "--out"; lean; "--force"; "--no-check" ]).Proc.status;
        (* a statement that is not well typed *)
        let broken = Fs.(dir / "broken.md") in
        is_ok "written" (Fs.write broken (Text.replace_all ~sub:"(n : Nat) : sumOdd n = n * n" ~by:"(n : Nat) : sumOdd n = true" (read blueprint)));
        let out = run_cli [ "blueprint"; "skeleton"; broken; "--json" ] in
        equal_int "it does not compile" 7 out.Proc.status;
        let problems = Json.list_or_empty "problems" (json_of out) in
        check "the error is laid at the door of the node" (List.exists (fun p -> Json.string_or "" "node" p = "odd-square") problems);
        Fs.remove_tree dir);
    case "P16-04-05 blueprint status reads the state of every node from Lean" (fun () ->
        let out = run_cli [ "blueprint"; "status"; blueprint; "--label"; "half done"; "--json" ] in
        equal_int "a report" 0 out.Proc.status;
        let status = json_of out in
        conforms "schemas/blueprint/status.schema.json" status;
        check "the snapshot the repository keeps" (Json.equal status (snapshot "2-half-done"));
        List.iter
          (fun (stage, name, label) ->
            let out = run_cli [ "blueprint"; "status"; blueprint; "--lean"; "fixtures/blueprint/lean/" ^ stage ^ ".lean"; "--label"; label; "--json" ] in
            check name (Json.equal (json_of out) (snapshot name)))
          [ ("Stage1", "1-skeleton", "skeleton"); ("Stage3", "3-one-proof-left", "one proof left"); ("Stage4", "4-finished", "finished") ];
        let text = run_cli [ "blueprint"; "status"; blueprint ] in
        contains "the table" text.Proc.stdout "odd-square     theorem     proved";
        contains "what a node waits for" text.Proc.stdout "two-triangles  lemma       stated   waits for tri-formula";
        contains "the counts" text.Proc.stdout "8 node(s): 3 done, 1 proved, 3 stated, 1 planned";
        equal_int "strict, with work left" 7 (run_cli [ "blueprint"; "status"; blueprint; "--strict"; "--quiet" ]).Proc.status;
        let dir = temp_dir "blueprint-status" in
        let svg = Fs.(dir / "status.svg") in
        let finished = run_cli [ "blueprint"; "status"; blueprint; "--lean"; "fixtures/blueprint/lean/Stage4.lean"; "--strict"; "--svg"; svg ] in
        equal_int "strict, with everything done" 0 finished.Proc.status;
        contains "where Lean and the blueprint disagree" finished.Proc.stdout "note: the blueprint lists tri-step, and the Lean text does not refer to it";
        contains "the picture" (is_ok "drawn" (Fs.read svg)) "8 node(s): 8 done, 0 proved, 0 stated, 0 planned";
        (* a blueprint with no Lean file beside it *)
        let alone = Fs.(dir / "alone.md") in
        is_ok "copied" (Fs.write alone (read blueprint));
        let out = run_cli [ "blueprint"; "status"; alone ] in
        contains "every node is planned" out.Proc.stdout "8 node(s): 0 done, 0 proved, 0 stated, 8 planned";
        Fs.remove_tree dir);
    case "P16-04-06 blueprint next lists what can be worked on, the most useful first" (fun () ->
        let out = run_cli [ "blueprint"; "next"; blueprint; "--blocked" ] in
        equal_int "a list" 0 out.Proc.status;
        equal_strings "the frontier and what waits"
          [ "1. tri-formula  lemma  to prove  Twice a triangular number  (2 node(s) rest on it)"; "2. odd-step  lemma  to prove  The next odd number  (1 node(s) rest on it)";
            "   two-triangles waits for tri-formula"; "   odd-triangles waits for two-triangles" ]
          (lines out);
        equal_int "at most one" 1 (List.length (lines (run_cli [ "blueprint"; "next"; blueprint; "--limit"; "1" ])));
        let done_ = run_cli [ "blueprint"; "next"; blueprint; "--lean"; "fixtures/blueprint/lean/Stage4.lean" ] in
        contains "nothing is left" done_.Proc.stdout "nothing is left: every node is done";
        let j = json_of (run_cli [ "blueprint"; "next"; blueprint; "--json" ]) in
        equal_strings "for a program" [ "tri-formula"; "odd-step" ] (Json.strings (Json.List (Json.list_or_empty "frontier" j)))) ]

(* ---- the search over declarations ---- *)

let lookup_tests =
  let from = [ "--from"; "fixtures/intel/extractions" ] in
  let hit_names j = List.map (fun h -> Json.string_or "" "name" (Option.get (Json.member "declaration" h))) (Json.list_or_empty "hits" j) in
  (* Rankings without the time each query took. *)
  let rec untimed = function
    | Json.Obj members -> Json.Obj (List.filter_map (fun (k, v) -> if k = "elapsed_ms" then None else Some (k, untimed v)) members)
    | Json.List items -> Json.List (List.map untimed items)
    | other -> other
  in
  [ case "P04-05-01 search answers from an export of declarations, and from Lean files read on the spot" (fun () ->
        let out = run_cli ([ "search"; "the"; "tail"; "of"; "a"; "sorted"; "list"; "--limit"; "3"; "--json" ] @ from) in
        equal_int "hits" 0 out.Proc.status;
        let j = json_of out in
        conforms "schemas/search/results.schema.json" j;
        equal_string "the declaration that was meant" "Maithic.Algo.Sorted.tail" (List.hd (hit_names j));
        equal_int "three hits" 3 (List.length (hit_names j));
        let text = run_cli ([ "search"; "a + b = b + a"; "--limit"; "1" ] @ from) in
        contains "notation is searched as its name" text.Proc.stdout " 1. Bad_Name  theorem  fixtures/intel/lean/Demo.lean:";
        contains "the count" text.Proc.stdout "1 hit(s) among 41 declaration(s)";
        (* the same declarations, read from the Lean files now *)
        let live = run_cli [ "search"; "insertion"; "sort"; "permutation"; "--in"; "lean/Maithic/Algo/Sort/Insertion.lean"; "--project"; "lean"; "--limit"; "1"; "--json" ] in
        equal_int "hits" 0 live.Proc.status;
        equal_strings "found in the file" [ "Maithic.Algo.insertionSort_perm" ] (hit_names (json_of live));
        equal_int "nothing found" 7 (run_cli ([ "search"; "cohomology" ] @ from)).Proc.status;
        equal_int "no declarations given" 2 (run_cli [ "search"; "sorted" ]).Proc.status;
        let missing = run_cli [ "search"; "sorted"; "--from"; "fixtures/nowhere" ] in
        equal_int "an export that is not there" 2 missing.Proc.status;
        contains "the code" (missing.Proc.stdout ^ missing.Proc.stderr) "MTH-SEARCH-0001");
    case "P04-05-01 search asks Lean about everything in scope when told to" (fun () ->
        let out = run_cli ([ "search"; "succ"; "mul"; "--lean"; "--limit"; "6"; "--json" ] @ from) in
        equal_int "hits" 0 out.Proc.status;
        let j = json_of out in
        check "a lemma of the core library is among them" (List.mem "Nat.succ_mul" (hit_names j));
        let core = List.find (fun h -> Json.string_or "" "name" (Option.get (Json.member "declaration" h)) = "Nat.succ_mul") (Json.list_or_empty "hits" j) in
        equal_strings "found by Lean" [ "lean" ] (List.map (fun f -> Json.string_or "" "retriever" f) (Json.list_or_empty "found_by" core));
        contains "with its statement" (Json.string_or "" "statement" (Option.get (Json.member "declaration" core))) "n.succ * m = n * m + m");
    case "P04-04-02 search answers a query set, and the rankings are the ones the repository keeps" (fun () ->
        let dir = temp_dir "lookup" in
        List.iter
          (fun (extra, kept) ->
            let out_file = Fs.(dir / kept) in
            let out = run_cli ([ "search"; "--queries"; "fixtures/search/queries.json"; "--rankings"; out_file ] @ extra @ from) in
            equal_int "answered" 0 out.Proc.status;
            contains "the count" out.Proc.stdout "answered 16 query(ies) over 41 declaration(s)";
            let rankings = Json.parse_exn (is_ok "written" (Fs.read out_file)) in
            conforms "schemas/search/rankings.schema.json" rankings;
            check kept (Json.equal (untimed rankings) (untimed (Json.parse_exn (read ("fixtures/search/" ^ kept))))))
          [ ([], "rankings-merged.json"); ([ "--only"; "lexical" ], "rankings-lexical.json"); ([ "--only"; "name" ], "rankings-name.json") ];
        Fs.remove_tree dir) ]

(* ---- the regression gate ---- *)

let gate_tests =
  let run_file = "bench/results/basics-portfolio.json" in
  [ case "P15-04-03 bench gate passes the committed run, fails a worse one with status 13, and stores a baseline when told to" (fun () ->
        let out = run_cli [ "bench"; "gate"; run_file ] in
        equal_int "the gate is passed" 0 out.Proc.status;
        contains "the verdict" out.Proc.stdout "basics  passed  verified 24 of 24 (baseline 24 of 24)  time x1.00  candidates x1.00";
        let verdicts = json_of (run_cli [ "bench"; "gate"; run_file; "--json" ]) in
        conforms "schemas/bench/gate.schema.json" verdicts;
        (* the same run with one problem lost *)
        let dir = temp_dir "gate" in
        let worse = Fs.(dir / "worse.json") in
        let lost =
          match Json.parse_exn (read run_file) with
          | Json.Obj members ->
            Json.Obj
              (List.map
                 (function
                   | "problems", Json.List (Json.Obj first :: rest) ->
                     ("problems", Json.List (Json.Obj (List.map (fun (k, v) -> if k = "status" then (k, Json.String "failed") else (k, v)) first) :: rest))
                   | member -> member)
                 members)
          | other -> other
        in
        is_ok "written" (Fs.write worse (Json.to_string lost));
        let out = run_cli [ "bench"; "gate"; worse ] in
        equal_int "a regression" 13 out.Proc.status;
        contains "the lost problem is named" out.Proc.stdout "1 problem(s) that the baseline verified are not verified: basics-001";
        contains "the gate says so" out.Proc.stdout "the gate is not passed";
        equal_int "one loss may be allowed" 0 (run_cli [ "bench"; "gate"; worse; "--max-regressions"; "1" ]).Proc.status;
        equal_int "and a share may be asked for" 13 (run_cli [ "bench"; "gate"; worse; "--max-regressions"; "1"; "--min-rate"; "0.99" ]).Proc.status;
        (* a store of its own *)
        let store = Fs.(dir / "baselines") in
        let none = run_cli [ "bench"; "gate"; run_file; "--baselines"; store ] in
        equal_int "no baseline yet" 2 none.Proc.status;
        contains "the code and the way out" (none.Proc.stdout ^ none.Proc.stderr) "MTH-BENCH-0003";
        let stored = run_cli [ "bench"; "gate"; worse; "--baselines"; store; "--update"; "--note"; "a worse day" ] in
        equal_int "stored" 0 stored.Proc.status;
        contains "what was stored" stored.Proc.stdout "stored the baseline of basics: 23 of 24 verified";
        conforms "schemas/bench/baseline.schema.json" (Json.parse_exn (is_ok "the file" (Fs.read Fs.(store / "basics.json"))));
        let better = run_cli [ "bench"; "gate"; run_file; "--baselines"; store ] in
        equal_int "against the worse baseline the committed run passes" 0 better.Proc.status;
        contains "and gains a problem" better.Proc.stdout "newly verified: basics-001";
        equal_int "a file that is not a run" 2 (run_cli [ "bench"; "gate"; "bench/suites/manifest.json" ]).Proc.status;
        Fs.remove_tree dir) ]

(* ---- verification of neural networks ---- *)

let nnv_tests =
  (* A job without what differs from one run to the next. *)
  let rec untimed = function
    | Json.Obj members -> Json.Obj (List.filter_map (fun (k, v) -> if List.mem k [ "elapsed_ms"; "verify_ms" ] then None else Some (k, untimed v)) members)
    | Json.List items -> Json.List (List.map untimed items)
    | other -> other
  in
  let verify ?(more = []) model property = run_cli ([ "nnv"; "verify"; "examples/nnv/" ^ model ^ ".json"; "examples/nnv/" ^ property ^ ".json" ] @ more) in
  [ case "P08-04-03 nnv verify has the verifier decide a property and Lean's kernel check the answer" (fun () ->
        need_julia ();
        List.iter
          (fun (model, property, status, code) ->
            let out = verify ~more:[ "--json" ] model property in
            equal_int (property ^ ": exit status") code out.Proc.status;
            let j = json_of out in
            conforms "schemas/nnv/job.schema.json" j;
            equal_string (property ^ ": status") status (Json.string_or "" "status" j);
            check (property ^ ": the job the repository keeps") (Json.equal (untimed j) (untimed (Json.parse_exn (read ("fixtures/nnv/jobs/" ^ property ^ ".json"))))))
          [ ("absolute-value", "bounded", "verified", 0); ("absolute-value", "too-tight", "refuted", 8); ("absolute-value", "beyond-the-bounds", "unknown", 7);
            ("small-classifier", "unit-square", "verified", 0) ];
        let text = verify "absolute-value" "too-tight" in
        contains "the counterexample" text.Proc.stdout "refuted: the input (3) is mapped to (3), outside the claimed box; Lean's kernel accepted Maithic.NNV.Job.property_fails";
        contains "what Lean was asked" text.Proc.stdout "¬Maithic.NN.Holds Maithic.NNV.Job.net Maithic.NNV.Job.input Maithic.NNV.Job.claimed");
    case "P08-04-02 the theorem that nnv verify writes is checked again by maithic check, on the standard axioms" (fun () ->
        need_julia ();
        let dir = temp_dir "nnv" in
        let lean = Fs.(dir / "UnitSquare.lean") in
        let out = verify ~more:[ "--write"; lean ] "small-classifier" "unit-square" in
        equal_int "verified" 0 out.Proc.status;
        same_file "the file the repository keeps" "fixtures/nnv/lean/UnitSquare.lean" lean;
        let again = run_cli [ "check"; lean; "--project"; "lean"; "--json" ] in
        equal_int "accepted again" 0 again.Proc.status;
        let theorems = Json.list_or_empty "theorems" (json_of again) in
        equal_int "one theorem" 1 (List.length theorems);
        equal_string "verified" "verified" (Json.string_or "" "status" (List.hd theorems));
        check "nothing compiled is trusted" (not (List.mem "Lean.ofReduceBool" (Json.strings (Json.List (Json.list_or_empty "axioms" (List.hd theorems))))));
        (* a certificate that was tampered with is refused by the kernel *)
        let tampered = Text.replace_all ~sub:"def claimed : Box := [⟨(-1), 1⟩]" ~by:"def claimed : Box := [⟨(-1), 0⟩]" (read "fixtures/nnv/lean/UnitSquare.lean") in
        is_ok "written" (Fs.write lean tampered);
        equal_int "a false claim does not pass" 7 (run_cli [ "check"; lean; "--project"; "lean" ]).Proc.status;
        equal_int "a file that is not a network" 2 (run_cli [ "nnv"; "verify"; "examples/nnv/bounded.json"; "examples/nnv/bounded.json" ]).Proc.status;
        Fs.remove_tree dir) ]

let algolab_tests =
  let task = "examples/algo/run-lengths.toml" in
  let synth more = run_cli ([ "algo"; "synth"; task ] @ more) in
  let recorded name = Json.parse_exn (read ("fixtures/algolab/" ^ name ^ ".json")) in
  let origins j = List.map (fun a -> Json.string_or "" "origin" (Option.get (Json.member "provenance" a))) (Json.list_or_empty "attempts" j) in
  [ case "P05-04-03 algo synth drafts a function, hears from Lean why two drafts fail, and ends with one that passes every example" (fun () ->
        let out = synth [ "--json" ] in
        equal_int "accepted" 0 out.Proc.status;
        let j = json_of out in
        conforms "schemas/algolab/synthesis.schema.json" j;
        equal_strings "a draft and two corrections" [ "draft"; "repair"; "repair" ] (origins j);
        check "the record the repository keeps" (Json.equal (without_account j) (without_account (recorded "run-lengths")));
        (match Json.list_or_empty "attempts" j with
         | [ first; second; third ] ->
           contains "Lean cannot see that the first draft ends" (String.concat "\n" (Json.strings (Json.List (Json.list_or_empty "errors" first)))) "fail to show termination";
           equal_strings "the second loses a run" [ "[(1, 2)]"; "[]"; "[(7, 1)]"; "[(4, 4)]"; "1" ] (List.map (Json.string_or "" "got") (Json.list_or_empty "examples" second));
           check "the third gets every example right" (List.for_all (Json.bool_or false "passed") (Json.list_or_empty "examples" third) && Json.bool_or false "accepted" third)
         | _ -> fail "three attempts");
        let text = synth [] in
        contains "the first attempt" text.Proc.stdout "attempt 1  (draft)  does not compile: line 2: fail to show termination for runLengths";
        contains "the second" text.Proc.stdout "runLengths [1, 1, 2, 3, 3, 3]  should give  [(1, 2), (2, 1), (3, 3)]  and gives  [(1, 2)]";
        contains "the verdict" text.Proc.stdout "accepted: runLengths compiles and gets 5 example(s) right, after 3 attempt(s)";
        let one = synth [ "--rounds"; "1"; "--quiet" ] in
        equal_int "with one correction nothing is accepted" 7 one.Proc.status;
        contains "and the run says so" one.Proc.stdout "failed: no implementation was accepted after 1 correction(s)");
    case "P05-04-04 algo synth judges an implementation that was written by hand, and writes an accepted one with its examples as checks" (fun () ->
        let dir = temp_dir "algolab" in
        let lean = Fs.(dir / "RunLengths.lean") in
        let lost = synth [ "--given"; "fixtures/algolab/given/loses-a-run.lean"; "--no-ai"; "--write"; lean; "--json" ] in
        equal_int "an implementation that gets examples wrong" 7 lost.Proc.status;
        check "the record the repository keeps" (Json.equal (without_account (json_of lost)) (without_account (recorded "loses-a-run")));
        check "nothing is written for it" (not (Fs.exists lean));
        let kept = synth [ "--given"; "fixtures/algolab/given/keeps-every-run.lean"; "--no-ai"; "--write"; lean ] in
        equal_int "one that gets them right" 0 kept.Proc.status;
        contains "after one attempt" kept.Proc.stdout "attempt 1  (given)  compiles; 5 of 5 example(s) right";
        let again = run_cli [ "check"; lean ] in
        equal_int "the written file compiles, examples included" 0 again.Proc.status;
        is_ok "written" (Fs.write lean (Text.replace_all ~sub:"== ([(7, 1)])" ~by:"== ([(7, 2)])" (is_ok "read" (Fs.read lean))));
        check "and an example that is changed no longer passes" ((run_cli [ "check"; lean ]).Proc.status <> 0);
        let replayed = synth [ "--write"; lean; "--quiet" ] in
        equal_int "the replayed run" 0 replayed.Proc.status;
        same_file "the file the repository keeps" "fixtures/algolab/lean/RunLengths.lean" lean;
        equal_int "--no-ai needs something to judge" 2 (synth [ "--no-ai" ]).Proc.status;
        let bad = run_cli [ "algo"; "synth"; "examples/even.lean" ] in
        equal_int "a file that is not a task" 2 bad.Proc.status;
        contains "with its code" bad.Proc.stderr "MTH-ALGOLAB-0001";
        Fs.remove_tree dir);
    case "P05-05-03 algo differential holds the compiled verified algorithms against the references, and they agree" (fun () ->
        need_julia ();
        let out = run_cli [ "algo"; "differential"; "--json" ] in
        equal_int "they agree" 0 out.Proc.status;
        let j = json_of out in
        conforms "schemas/algolab/differential.schema.json" j;
        check "the report the repository keeps" (Json.equal j (recorded "differential"));
        equal_int "nineteen algorithms" 19 (List.length (Json.list_or_empty "algorithms" j));
        let some = run_cli [ "algo"; "differential"; "--seed"; "7"; "--count"; "40"; "--algorithm"; "gcd"; "--algorithm"; "path" ] in
        equal_int "another seed, two algorithms" 0 some.Proc.status;
        contains "in a line" some.Proc.stdout "agree: 40 case(s) over 2 algorithm(s), and every answer is accepted by its reference";
        (* a program that answers wrongly is caught *)
        let dir = temp_dir "differential" in
        let liar = Fs.(dir / "liar") in
        is_ok "written" (Fs.write liar "#!/bin/sh\nwhile read line; do echo 1; done\n");
        Unix.chmod liar 0o755;
        let caught = run_cli [ "algo"; "differential"; "--exe"; liar; "--count"; "38"; "--json" ] in
        equal_int "status 7" 7 caught.Proc.status;
        check "with the answers that differ" (Json.int_or 0 "mismatches" (json_of caught) > 0 && not (Json.bool_or true "agrees" (json_of caught)));
        equal_int "a program that is not there" 4 (run_cli [ "algo"; "differential"; "--exe"; Fs.(dir / "nothing") ]).Proc.status;
        Fs.remove_tree dir) ]

let () =
  run "integration"
    [ ("bridge sessions", session_tests); ("bridge", bridge_tests); ("proving engine", engine_tests); ("search", search_tests);
      ("oracles", oracle_tests); ("command", cli_tests); ("search records", view_tests); ("code, datasets and review", insight_tests);
      ("counterexamples", refute_tests); ("guide", guide_tests); ("repair", mend_tests); ("formalize", formalize_tests); ("council", council_tests); ("blueprint", blueprint_tests); ("lookup", lookup_tests); ("gate", gate_tests); ("networks", nnv_tests); ("algorithms", algolab_tests); ("contracts", contract_tests) ]
