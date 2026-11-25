(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the verification job: a verifier and a Lean are stood in for by functions, so that every
   way a job can go is taken. The real verifier and the real kernel meet in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger
open Maithic_nnv
open Testkit

let recorded name = Support.json ("fixtures/nnv/jobs/" ^ name ^ ".json")
let verdict_of name = Option.get (Json.member "verdict" (recorded name))

(* A Lean that accepts the theorem it is asked about, on the axioms given. *)
let accepting ?(axioms = [ "propext"; "Quot.sound" ]) ?(seen = ref []) () : Nnv_job.checker =
 fun ~source ~theorem ->
  seen := !seen @ [ (theorem, source) ];
  Ok { Recheck.passed = true; declaration = theorem; statement = "the statement"; kind = "theorem"; axioms; errors = 0; first_error = ""; elapsed_ms = 7; lean = "4" }

let rejecting said : Nnv_job.checker =
 fun ~source:_ ~theorem ->
  Ok { Recheck.passed = false; declaration = theorem; statement = ""; kind = ""; axioms = []; errors = 1; first_error = said; elapsed_ms = 3; lean = "4" }

let run_job ?(check = accepting ()) verdict =
  Nnv_job.run ~verify:(fun () -> Ok verdict) ~check ~policy:Trust_policy.default ~model:"a network" ~property:"a property"

let job ?check verdict = is_ok "a job" (run_job ?check verdict)

let text_tests =
  [ case "P08-04-02 the claim of a verifier becomes a Lean theorem that the certificate checker proves in the kernel" (fun () ->
        check "exact numbers" (List.for_all Nnv_text.is_exact [ "0"; "-7"; "3/4"; "-22/7"; "12345678901234567890/3" ]);
        check "and what is not one" (not (List.exists Nnv_text.is_exact [ ""; "0.5"; "1/0"; "1/"; "--1"; "1e3"; "1/2/3"; "x"; "1 "; "(1)"; "1 := sorry" ]));
        check "a negative number is put in brackets" (Nnv_text.lean_number "-7/2" = Ok "(-7/2)" && Nnv_text.lean_number "5" = Ok "5");
        let lean = Option.get (is_ok "Lean text" (Nnv_text.of_verdict (verdict_of "unit-square"))) in
        equal_string "the theorem" "Maithic.NNV.Job.property_holds" lean.Nnv_text.theorem;
        equal_string "the file the repository keeps" (Support.read "fixtures/nnv/lean/UnitSquare.lean") lean.Nnv_text.source;
        contains "the network, exactly" lean.Nnv_text.source "{ neurons := [⟨[1/10, (-3/10)], 1/20⟩, ⟨[7/10, 1/5], (-3/20)⟩, ⟨[(-2/5), 3/5], 1/10⟩], activation := .relu }";
        contains "the claim" lean.Nnv_text.source "theorem property_holds : Holds net input claimed :=\n  checkProperty_sound (cert := certificate) (by decide +kernel)";
        check "nothing is assumed" (not (Text.contains lean.Nnv_text.source "sorry") && not (Text.contains lean.Nnv_text.source "native_decide"));
        let refutation = Option.get (is_ok "Lean text" (Nnv_text.of_verdict (verdict_of "too-tight"))) in
        equal_string "the theorem of a counterexample" "Maithic.NNV.Job.property_fails" refutation.Nnv_text.theorem;
        contains "the point" refutation.Nnv_text.source "def point : List Rat := [3]";
        contains "and what it shows" refutation.Nnv_text.source "theorem property_fails : ¬ Holds net input claimed :=\n  not_holds_of_counterexample (xs := point) (by decide +kernel)";
        check "a verdict that claims nothing has no theorem" (Nnv_text.of_verdict (verdict_of "beyond-the-bounds") = Ok None));
    case "P08-04-02 nothing but exact numbers reaches the Lean file" (fun () ->
        let with_number text =
          Json.Obj
            [ ("verdict", Json.String "verified"); ("network", Json.List [ Json.Obj [ ("activation", Json.String "relu"); ("neurons", Json.List [ Json.Obj [ ("weights", Json.List [ Json.String text ]); ("bias", Json.String "0") ] ]) ] ]);
              ("input", Json.List [ Json.List [ Json.String "0"; Json.String "1" ] ]); ("output", Json.List [ Json.List [ Json.String "0"; Json.String "1" ] ]);
              ("boxes", Json.List [ Json.List [ Json.List [ Json.String "0"; Json.String "1" ] ] ]) ]
        in
        check "a sound verdict" (Result.is_ok (Nnv_text.of_verdict (with_number "1/2")));
        contains "Lean text in the place of a number" (is_error "refused" (Nnv_text.of_verdict (with_number "0⟩], activation := .relu }] theorem x : False := sorry --"))) "is not an exact number";
        contains "a decimal" (is_error "refused" (Nnv_text.of_verdict (with_number "0.5"))) "'0.5' is not an exact number";
        contains "an unknown verdict" (is_error "refused" (Nnv_text.of_verdict (Json.Obj [ ("verdict", Json.String "maybe"); ("network", Json.List []); ("input", Json.List []); ("output", Json.List []) ]))) "'maybe' is not a verdict";
        let pointless = match verdict_of "too-tight" with Json.Obj m -> Json.Obj (List.filter (fun (k, _) -> k <> "counterexample") m) | other -> other in
        contains "a violation without its point" (is_error "refused" (Nnv_text.of_verdict pointless)) "without a counterexample") ]

let job_tests =
  [ case "P08-04-01 the verifier is asked first, and Lean is asked about what it claims" (fun () ->
        let order = ref [] and seen = ref [] in
        let verify () =
          order := !order @ [ "verifier" ];
          Ok (verdict_of "bounded")
        in
        let ask ~source ~theorem =
          order := !order @ [ "Lean" ];
          accepting ~seen () ~source ~theorem
        in
        let j = is_ok "a job" (Nnv_job.run ~verify ~check:ask ~policy:Trust_policy.default ~model:"absolute value" ~property:"bounded") in
        equal_strings "in this order" [ "verifier"; "Lean" ] !order;
        check "the status" (j.Nnv_job.status = Status.Verified);
        equal_int "exit status" 0 (Exit_code.to_int (Nnv_job.exit_code j));
        equal_string "what Lean was asked about" "Maithic.NNV.Job.property_holds" (fst (List.hd !seen));
        contains "with the certificate of the verifier" (snd (List.hd !seen)) "def certificate : Certificate := [\n  [⟨0, 3⟩, ⟨0, 2⟩],\n  [⟨0, 5⟩]]";
        check "the file that was checked is kept" (j.Nnv_job.lean = Some (snd (List.hd !seen)));
        contains "in words" (Nnv_job.summary j) "verified: Lean's kernel accepted Maithic.NNV.Job.property_holds (axioms: propext, Quot.sound)";
        let refuted = job (verdict_of "too-tight") in
        check "a counterexample that Lean accepts refutes" (refuted.Nnv_job.status = Status.Refuted);
        equal_int "exit status" 8 (Exit_code.to_int (Nnv_job.exit_code refuted));
        contains "in words" (Nnv_job.summary refuted) "refuted: the input (3) is mapped to (3), outside the claimed box";
        let asked = ref [] in
        let unknown = job ~check:(accepting ~seen:asked ()) (verdict_of "beyond-the-bounds") in
        check "no claim, no status" (unknown.Nnv_job.status = Status.Unknown && unknown.Nnv_job.check = None);
        check "and Lean is not asked" (!asked = []);
        equal_int "exit status" 7 (Exit_code.to_int (Nnv_job.exit_code unknown));
        contains "the reason of the verifier" unknown.Nnv_job.reason "the bounds are too loose");
    case "P08-04-01 a claim that Lean does not accept settles nothing" (fun () ->
        let j = job ~check:(rejecting "The proposition is false\n  more") (verdict_of "bounded") in
        check "failed" (j.Nnv_job.status = Status.Failed);
        equal_int "exit status" 7 (Exit_code.to_int (Nnv_job.exit_code j));
        equal_string "what happened"
          "the verifier claims that the property holds, and Lean did not accept the theorem that says so: The proposition is false" j.Nnv_job.reason;
        let c = Option.get j.Nnv_job.check in
        check "the check says so" ((not c.Nnv_job.accepted) && c.Nnv_job.error <> "");
        let wrong_point = job ~check:(rejecting "it reduces to false") (verdict_of "too-tight") in
        contains "a counterexample that is none" wrong_point.Nnv_job.reason "the verifier claims that the property fails, and Lean did not accept";
        (* a theorem the kernel accepted on an axiom the policy does not allow is not verified either *)
        let native = job ~check:(accepting ~axioms:[ "propext"; "Lean.ofReduceBool" ] ()) (verdict_of "bounded") in
        check "the trust policy has the last word" (native.Nnv_job.status = Status.Failed);
        contains "and says why" native.Nnv_job.reason "the proof trusts compiled evaluation, which the policy does not allow";
        contains "no verifier" (is_error "no job" (Nnv_job.run ~verify:(fun () -> Error "Julia is not installed") ~check:(accepting ()) ~policy:Trust_policy.default ~model:"m" ~property:"p")) "the verifier gave no verdict: Julia is not installed";
        contains "not a verdict" (is_error "no job" (run_job (Json.Obj [ ("schema", Json.String "maithic.nnv.network/1") ]))) "not a verdict";
        contains "Lean out of reach" (is_error "no job" (run_job ~check:(fun ~source:_ ~theorem:_ -> Error "the Bridge is gone") (verdict_of "bounded"))) "Lean could not be asked: the Bridge is gone");
    case "P08-04-03 a job is a document and a few lines" (fun () ->
        let schema = Json_schema.of_json (Support.json "schemas/nnv/job.schema.json") in
        List.iter
          (fun name ->
            let j = job (verdict_of name) in
            equal_strings name [] (List.map Json_schema.error_to_string (Json_schema.validate schema (Nnv_job.to_json j)));
            equal_strings ("the recorded job " ^ name) [] (List.map Json_schema.error_to_string (Json_schema.validate schema (recorded name)));
            equal_string ("its status " ^ name) (Json.string_or "" "status" (recorded name)) (Status.to_string j.Nnv_job.status))
          [ "bounded"; "too-tight"; "beyond-the-bounds"; "unit-square" ];
        let shown = Nnv_job.render (job (verdict_of "unit-square")) in
        contains "the network" shown "network   a network: 3 layer(s), 6 neuron(s)";
        contains "the boxes" shown "  inputs in   [0, 1] x [0, 1]\n  outputs in  [-1, 1]";
        contains "the claim" shown "verifier  claims verified";
        contains "what Lean said" shown "Lean      accepted Maithic.NNV.Job.property_holds";
        contains "without a claim" (Nnv_job.render (job (verdict_of "beyond-the-bounds"))) "Lean      was not asked: there is no claim to check") ]

let () = run "nnv" [ ("Lean text", text_tests); ("jobs", job_tests) ]
