(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger
open Testkit

let facts ?(found = true) ?(kind = "theorem") ?(errors = 0) ?(rechecked = true) axioms =
  { Trust_policy.declaration = "t"; found; kind; axioms; errors; rechecked }

let status policy f = Status.to_string (Trust_policy.decide policy f).Trust_policy.status
let default = Trust_policy.default

let tests =
  [ case "P18-03-05 the default policy allows the standard axioms only and requires a re-check" (fun () ->
        equal_strings "axioms" [ "propext"; "Quot.sound"; "Classical.choice" ] default.Trust_policy.allowed_axioms;
        check "native" (not default.Trust_policy.allow_native);
        check "recheck" default.Trust_policy.require_recheck);
    case "P18-03-03 a kernel-checked theorem on allowed axioms is verified" (fun () ->
        equal_string "no axioms" "verified" (status default (facts []));
        equal_string "standard" "verified" (status default (facts [ "propext"; "Quot.sound"; "Classical.choice" ])));
    case "P18-03-02 without the required re-check the result stays at checked" (fun () ->
        let d = Trust_policy.decide default (facts ~rechecked:false [ "propext" ]) in
        equal_string "status" "checked" (Status.to_string d.Trust_policy.status);
        contains "reason" (String.concat " " d.Trust_policy.reasons) "re-check";
        equal_string "not required" "verified" (status { default with Trust_policy.require_recheck = false } (facts ~rechecked:false [])));
    case "P18-03-01 an axiom outside the policy fails the result and is named" (fun () ->
        let d = Trust_policy.decide default (facts [ "propext"; "shortcut" ]) in
        equal_string "status" "failed" (Status.to_string d.Trust_policy.status);
        equal_strings "violations" [ "shortcut" ] d.Trust_policy.violations;
        contains "reason" (String.concat " " d.Trust_policy.reasons) "axioms outside the policy: shortcut";
        let constructive = { default with Trust_policy.allowed_axioms = [ "propext"; "Quot.sound" ] } in
        equal_string "choice disallowed" "failed" (status constructive (facts [ "Classical.choice" ]));
        equal_string "custom axiom allowed" "verified"
          (status { default with Trust_policy.allowed_axioms = "shortcut" :: default.Trust_policy.allowed_axioms } (facts [ "shortcut" ])));
    case "P18-03-03 sorry, native evaluation, errors and non-theorems are never verified" (fun () ->
        contains "sorry" (String.concat " " (Trust_policy.decide default (facts [ "sorryAx" ])).Trust_policy.reasons) "unfinished";
        equal_string "native" "failed" (status default (facts [ "t._native.native_decide.ax_1_1" ]));
        equal_string "native allowed" "verified" (status { default with Trust_policy.allow_native = true } (facts [ "t._native.native_decide.ax_1_1" ]));
        equal_string "errors" "failed" (status default (facts ~errors:1 []));
        equal_string "definition" "failed" (status default (facts ~kind:"def" []));
        equal_string "axiom declaration" "failed" (status default (facts ~kind:"axiom" []));
        equal_string "missing" "failed" (status default (facts ~found:false [])));
    case "P18-03-04 a decision records its status, reasons and violations" (fun () ->
        equal ~show:Json.to_string "json"
          (Json.parse_exn {|{"status":"failed","reasons":["axioms outside the policy: cheat"],"violations":["cheat"]}|})
          (Trust_policy.decision_to_json (Trust_policy.decide default (facts [ "cheat" ]))));
    case "P18-03-06 policy files load, keep defaults for missing members and reject mistakes" (fun () ->
        let parse text = Result.bind (Toml.parse text) Trust_policy.of_json in
        let p = is_ok "parse" (parse "[trust]\nname = \"constructive\"\nallowed_axioms = [\"propext\", \"Quot.sound\"]\n") in
        equal_string "name" "constructive" p.Trust_policy.name;
        equal_strings "axioms" [ "propext"; "Quot.sound" ] p.Trust_policy.allowed_axioms;
        check "defaults kept" (p.Trust_policy.require_recheck && not p.Trust_policy.allow_native);
        contains "sorry" (is_error "sorry" (parse "[trust]\nallowed_axioms = [\"sorryAx\"]\n")) "cannot allow";
        contains "unknown" (is_error "unknown" (parse "[trust]\nallow_everything = true\n")) "unknown trust policy setting";
        contains "type" (is_error "type" (parse "[trust]\nallowed_axioms = \"propext\"\n")) "list of strings";
        let shipped = is_ok "shipped policy" (Trust_policy.load (Support.path "config/policies/default.toml")) in
        check "the shipped default file equals the built-in default" (shipped = default));
    case "classifies axioms as the Lean audit does" (fun () ->
        check "standard" (List.for_all (fun a -> Trust_policy.classify a = Trust_policy.Standard) [ "propext"; "Quot.sound"; "Classical.choice" ]);
        check "sorry" (Trust_policy.classify "sorryAx" = Trust_policy.Sorry);
        check "native" (Trust_policy.classify "foo._native.native_decide.ax_1" = Trust_policy.Native && Trust_policy.classify "Lean.ofReduceBool" = Trust_policy.Native);
        check "other" (Trust_policy.classify "MyProject.assumption" = Trust_policy.Other));
    property "verified requires a theorem, no error, a re-check and only allowed axioms"
      (Gen.pair
         (Gen.list ~max:4 (Gen.one_of [ "propext"; "Quot.sound"; "Classical.choice"; "sorryAx"; "cheat"; "x._native.decide.ax_1" ]))
         (Gen.pair (Gen.pair Gen.bool Gen.bool) (Gen.pair (Gen.int 0 1) (Gen.one_of [ "theorem"; "def"; "axiom" ]))))
      (fun (axioms, ((found, rechecked), (errors, kind))) ->
        let d = Trust_policy.decide default { Trust_policy.declaration = "t"; found; kind; axioms; errors; rechecked } in
        let should =
          found && kind = "theorem" && errors = 0 && rechecked
          && List.for_all (fun a -> List.mem a default.Trust_policy.allowed_axioms) axioms
        in
        (d.Trust_policy.status = Status.Verified) = should) ]

let () = run "ledger" [ ("trust policy", tests) ]
