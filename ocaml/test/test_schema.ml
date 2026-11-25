(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Testkit

let status_tests =
  [ case "the vocabulary has exactly eight labels that round-trip" (fun () ->
        equal_strings "labels"
          [ "verified"; "checked"; "refuted"; "conjectured"; "unverified"; "unknown"; "failed"; "abstained" ]
          (List.map Status.to_string Status.all);
        List.iter (fun s -> check "round trip" (Status.of_string (Status.to_string s) = Some s)) Status.all;
        check "unknown word" (Status.of_string "proved" = None));
    case "only verified counts as established" (fun () ->
        equal_strings "established" [ "verified" ] (List.map Status.to_string (List.filter Status.is_established Status.all)));
    case "the weakest part decides the status of a whole" (fun () ->
        check "verified + unverified" (Status.weakest Status.Verified Status.Unverified = Status.Unverified);
        check "verified + checked" (Status.weakest Status.Checked Status.Verified = Status.Checked);
        check "verified + failed" (Status.weakest Status.Verified Status.Failed = Status.Failed);
        check "all verified" (Status.weakest_of [ Status.Verified; Status.Verified ] = Status.Verified);
        check "empty" (Status.weakest_of [] = Status.Verified));
    property "weakest is commutative, associative and idempotent"
      (Gen.pair (Gen.one_of Status.all) (Gen.pair (Gen.one_of Status.all) (Gen.one_of Status.all)))
      (fun (a, (b, c)) ->
        Status.weakest a b = Status.weakest b a
        && Status.weakest a (Status.weakest b c) = Status.weakest (Status.weakest a b) c
        && Status.weakest a a = a
        && Status.trust_rank (Status.weakest a b) = min (Status.trust_rank a) (Status.trust_rank b)) ]

let code_tests =
  [ case "P00-01-04 exit codes are the documented table" (fun () ->
        equal ~show:(fun l -> String.concat "," (List.map string_of_int l)) "codes" (List.init 14 Fun.id) (List.map Exit_code.to_int Exit_code.all);
        equal_string "0" "Success" (Exit_code.describe Exit_code.Success);
        equal_string "7" "Abstained or not verified" (Exit_code.describe Exit_code.Not_verified);
        equal_string "13" "Regression gate failed" (Exit_code.describe Exit_code.Regression);
        (* The table in the documentation is the same table. *)
        let doc = Support.read "docs/reference/exit-codes.md" in
        List.iter
          (fun c -> contains "documented" doc (Printf.sprintf "| %d | %s |" (Exit_code.to_int c) (Exit_code.describe c)))
          Exit_code.all);
    case "P00-01-04 a status maps to one exit code" (fun () ->
        equal_int "verified" 0 (Exit_code.to_int (Exit_code.of_status Status.Verified));
        equal_int "refuted" 8 (Exit_code.to_int (Exit_code.of_status Status.Refuted));
        List.iter
          (fun s -> equal_int (Status.to_string s) 7 (Exit_code.to_int (Exit_code.of_status s)))
          [ Status.Checked; Status.Failed; Status.Unknown; Status.Abstained; Status.Unverified; Status.Conjectured ]);
    case "P00-01-06 an error carries a code, a cause and a next step" (fun () ->
        let f = Fault.make ~area:"gateway" ~number:7 ~exit:Exit_code.Provider ~hint:"Record the cassette." "no cassette" in
        equal_string "code" "MTH-GATEWAY-0007" f.Fault.code;
        equal_string "text" "MTH-GATEWAY-0007: no cassette\n  hint: Record the cassette." (Fault.to_string f);
        equal ~show:Json.to_string "json"
          (Json.parse_exn {|{"code":"MTH-GATEWAY-0007","message":"no cassette","hint":"Record the cassette.","exit":5}|})
          (Fault.to_json f);
        check "shape" (Fault.is_code "MTH-BRIDGE-0001" && not (Fault.is_code "MTH-bridge-1") && not (Fault.is_code "ERR-BRIDGE-0001"))) ]

let cap_tests =
  [ case "parses and prints capability identifiers" (fun () ->
        let id = Option.get (Cap_id.of_string ("P07-03-" ^ "02")) in
        equal_string "print" ("P07-03-" ^ "02") (Cap_id.to_string id);
        equal_string "group" "P07-03" (Cap_id.group_id id);
        equal_string "pillar" "P07" (Cap_id.pillar_id id);
        List.iter
          (fun bad -> check bad (Cap_id.of_string bad = None))
          [ "P19-01-01"; "P00-00-01"; "P00-01-00"; "p00-01-01"; "P0-1-1"; "P00_01_01"; "P00-01-1"; "" ]);
    case "identifiers are ordered by pillar, then group, then place" (fun () ->
        let id s = Option.get (Cap_id.of_string s) in
        check "by pillar" (Cap_id.compare (id "P01-09-09") (id "P02-01-01") < 0);
        check "by group" (Cap_id.compare (id "P01-01-09") (id "P01-02-01") < 0);
        check "by place" (Cap_id.compare (id "P01-01-01") (id "P01-01-02") < 0);
        equal_int "nineteen pillars" 19 Cap_id.pillars);
    case "finds identifiers in running text at word boundaries" (fun () ->
        let first = "P01-02-" ^ "03" and second = "P18-10-" ^ "25" in
        equal_strings "scan" [ first; second ]
          (List.map Cap_id.to_string (Cap_id.scan (Printf.sprintf "see %s, not X%s or %s1; also (%s)." first first first second)))) ]

(* ---- JSON Schema ---- *)

let schema text = Json_schema.of_json (Json.parse_exn text)
let errors_of text instance = List.map Json_schema.error_to_string (Json_schema.validate (schema text) (Json.parse_exn instance))
let accepts text instance = check (Printf.sprintf "%s accepts %s" text instance) (errors_of text instance = [])
let rejects text instance fragment =
  match errors_of text instance with
  | [] -> fail (Printf.sprintf "%s accepted %s" text instance)
  | errors -> contains (Printf.sprintf "%s on %s" text instance) (String.concat "\n" errors) fragment

let pattern_ok pattern text = match Json_schema.pattern_matches pattern text with Ok b -> b | Error e -> fail e

let json_schema_tests =
  [ case "types are told apart, and an integer is a number" (fun () ->
        List.iter
          (fun (name, instance) -> accepts (Printf.sprintf {|{"type":"%s"}|} name) instance)
          [ ("null", "null"); ("boolean", "true"); ("object", "{}"); ("array", "[]"); ("string", {|"x"|}); ("integer", "3"); ("integer", "3.0");
            ("number", "3"); ("number", "3.5") ];
        rejects {|{"type":"integer"}|} "3.5" "expected integer, found number";
        rejects {|{"type":"string"}|} "3" "$: expected string, found integer";
        accepts {|{"type":["string","null"]}|} "null";
        rejects {|{"type":["string","null"]}|} "1" "expected string or null";
        accepts "true" "[1]";
        rejects "false" "[1]" "no value is allowed here");
    case "objects: properties, required members and members that are not allowed" (fun () ->
        let s = {|{"type":"object","properties":{"name":{"type":"string"},"n":{"type":"integer","minimum":0}},"required":["name"],"additionalProperties":false}|} in
        accepts s {|{"name":"a"}|};
        accepts s {|{"name":"a","n":0}|};
        rejects s {|{"n":1}|} {|$: the member "name" is missing|};
        rejects s {|{"name":1}|} "$.name: expected string";
        rejects s {|{"name":"a","n":-1}|} "$.n: must be at least 0";
        rejects s {|{"name":"a","extra":true}|} {|the member "extra" is not allowed|};
        let open_ = {|{"properties":{"name":{"type":"string"}},"additionalProperties":{"type":"integer"}}|} in
        accepts open_ {|{"name":"a","x":1,"y":2}|};
        rejects open_ {|{"x":"one"}|} "$.x: expected integer";
        rejects {|{"minProperties":1}|} "{}" "fewer than 1";
        equal_int "every failure is reported" 3 (List.length (errors_of s {|{"n":-1,"extra":1}|})));
    case "arrays: items, leading items, sizes and uniqueness" (fun () ->
        accepts {|{"items":{"type":"integer"}}|} "[1,2,3]";
        rejects {|{"items":{"type":"integer"}}|} {|[1,"two",3]|} "$[1]: expected integer";
        let pair = {|{"prefixItems":[{"type":"string"},{"type":"integer"}],"items":false,"minItems":2}|} in
        accepts pair {|["a",1]|};
        rejects pair {|["a"]|} "has 1 items, fewer than 2";
        rejects pair {|["a",1,2]|} "$[2]: no value is allowed here";
        rejects pair {|[1,1]|} "$[0]: expected string";
        rejects {|{"maxItems":1}|} "[1,2]" "more than 1";
        accepts {|{"uniqueItems":true}|} {|[1,2,{"a":1},{"a":2}]|};
        rejects {|{"uniqueItems":true}|} {|[{"a":1,"b":2},{"b":2,"a":1}]|} "the same item twice");
    case "strings and numbers: lengths, patterns and bounds" (fun () ->
        accepts {|{"minLength":2,"maxLength":3}|} {|"ab"|};
        rejects {|{"minLength":2}|} {|"a"|} "shorter than 2";
        rejects {|{"maxLength":1}|} {|"ab"|} "longer than 1";
        accepts {|{"maxLength":1}|} "\"\xC3\xA9\"";
        accepts {|{"pattern":"^-?[0-9]+(/[0-9]+)?$"}|} {|"-7/2"|};
        rejects {|{"pattern":"^-?[0-9]+(/[0-9]+)?$"}|} {|"0.5"|} "does not match the pattern";
        accepts {|{"minimum":1,"maximum":2}|} "1.5";
        rejects {|{"exclusiveMinimum":0}|} "0" "must be greater than 0";
        rejects {|{"exclusiveMaximum":1}|} "1" "must be less than 1";
        accepts {|{"minimum":1}|} {|"not a number, so the bound does not apply"|});
    case "enum, const and the combinators" (fun () ->
        accepts {|{"enum":["a",1,null]}|} "null";
        rejects {|{"enum":["a","b"]}|} {|"c"|} {|"c" is not one of "a", "b"|};
        accepts {|{"const":{"a":[1,2]}}|} {|{"a":[1,2]}|};
        rejects {|{"const":"x"}|} {|"y"|} {|expected "x"|};
        accepts {|{"allOf":[{"type":"integer"},{"minimum":3}]}|} "3";
        rejects {|{"allOf":[{"type":"integer"},{"minimum":3}]}|} "2" "at least 3";
        accepts {|{"anyOf":[{"type":"string"},{"type":"integer"}]}|} "1";
        rejects {|{"anyOf":[{"type":"string"},{"type":"integer"}]}|} "1.5" "matches none of the 2 alternatives";
        accepts {|{"oneOf":[{"type":"integer"},{"minimum":10}]}|} "3";
        rejects {|{"oneOf":[{"type":"integer"},{"minimum":10}]}|} "12" "matches 2 of the 2 alternatives";
        rejects {|{"oneOf":[{"type":"string"},{"type":"null"}]}|} "1" "matches 0 of the 2";
        accepts {|{"not":{"type":"string"}}|} "1";
        rejects {|{"not":{"type":"string"}}|} {|"s"|} "must not match";
        let conditional = {|{"if":{"properties":{"kind":{"const":"a"}}},"then":{"required":["x"]},"else":{"required":["y"]}}|} in
        accepts conditional {|{"kind":"a","x":1}|};
        rejects conditional {|{"kind":"a","y":1}|} {|the member "x" is missing|};
        accepts conditional {|{"kind":"b","y":1}|};
        rejects conditional {|{"kind":"b","x":1}|} {|the member "y" is missing|});
    case "references resolve within a file and across files" (fun () ->
        let tree = {|{"$defs":{"node":{"type":"object","properties":{"value":{"type":"integer"},"children":{"type":"array","items":{"$ref":"#/$defs/node"}}},"required":["value"]}},"$ref":"#/$defs/node"}|} in
        accepts tree {|{"value":1,"children":[{"value":2},{"value":3,"children":[]}]}|};
        rejects tree {|{"value":1,"children":[{"value":2},{"children":[]}]}|} {|$.children[1]: the member "value" is missing|};
        rejects {|{"$ref":"#/$defs/absent"}|} "1" "points at nothing";
        rejects {|{"$ref":"#"}|} "1" "refers to itself without end";
        let dir = temp_dir "schemas" in
        ignore (is_ok "mkdir" (Fs.write Fs.(dir / "shared" / "common.schema.json") {|{"$defs":{"status":{"enum":["verified","failed"]}}}|}));
        ignore (is_ok "write" (Fs.write Fs.(dir / "run" / "run.schema.json") {|{"type":"object","properties":{"status":{"$ref":"../shared/common.schema.json#/$defs/status"}}}|}));
        let run = is_ok "load" (Json_schema.load Fs.(dir / "run" / "run.schema.json")) in
        check "conforms" (Json_schema.conforms run (Json.parse_exn {|{"status":"verified"}|}) = Ok ());
        contains "error" (is_error "conforms" (Json_schema.conforms run (Json.parse_exn {|{"status":"proven"}|}))) {|$.status: "proven" is not one of|};
        check "the schema is sound" (Json_schema.lint run = []);
        ignore (is_ok "write" (Fs.write Fs.(dir / "run" / "broken.schema.json") {|{"$ref":"missing.schema.json"}|}));
        let broken = is_ok "load" (Json_schema.load Fs.(dir / "run" / "broken.schema.json")) in
        contains "a missing file" (String.concat "\n" (List.map Json_schema.error_to_string (Json_schema.lint broken))) "names a file that cannot be read";
        check "a missing schema file" (Result.is_error (Json_schema.load Fs.(dir / "absent.json"))));
    case "a schema with a mistake in it is reported by the linter" (fun () ->
        let complaints text = String.concat "\n" (List.map Json_schema.error_to_string (Json_schema.lint (schema text))) in
        check "a sound schema" (complaints {|{"$schema":"https://json-schema.org/draft/2020-12/schema","title":"t","type":"object","properties":{"a":{"type":"string","pattern":"^a+$"}},"required":["a"],"additionalProperties":false}|} = "");
        contains "a misspelt keyword" (complaints {|{"type":"object","require":["a"]}|}) {|the keyword "require" is not known|};
        contains "nested" (complaints {|{"properties":{"a":{"items":{"typ":"string"}}}}|}) {|$.properties.a.items: the keyword "typ" is not known|};
        contains "a type that does not exist" (complaints {|{"type":"float"}|}) {|"float" is not a type|};
        contains "an empty enum" (complaints {|{"enum":[]}|}) "enum needs a list that is not empty";
        contains "a bad pattern" (complaints {|{"pattern":"(unclosed"}|}) "a group is not closed";
        contains "a dangling reference" (complaints {|{"$defs":{"a":{"$ref":"#/$defs/b"}}}|}) "points at nothing";
        contains "required but forbidden" (complaints {|{"properties":{"a":{}},"required":["b"],"additionalProperties":false}|}) {|"b" is required and not allowed|};
        contains "a count that is not one" (complaints {|{"minItems":-1}|}) "minItems needs a whole number";
        contains "a bound that is not a number" (complaints {|{"minimum":"0"}|}) "minimum needs a number";
        contains "definitions are linted too" (complaints {|{"$defs":{"a":{"tipe":"x"}}}|}) "$.$defs.a");
    case "patterns: classes, anchors, groups, alternation and repetition" (fun () ->
        List.iter
          (fun (pattern, text, expected) -> check (Printf.sprintf "%s on %S" pattern text) (pattern_ok pattern text = expected))
          [ ("abc", "xxabcxx", true); ("abc", "ab", false); ("^abc$", "abc", true); ("^abc$", "abcd", false); ("a.c", "a-c", true);
            ("[a-c]+", "xbx", true); ("^[a-c]+$", "abd", false); ("^[^0-9]+$", "abc", true); ("^[^0-9]+$", "a1", false);
            ("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", "2026-11-22T21:30:05Z", true);
            ("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", "2026-11-22 21:30:05", false);
            ("^MTH-[A-Z]+-[0-9]{4}$", "MTH-BRG-0104", true); ("^MTH-[A-Z]+-[0-9]{4}$", "MTH-BRG-104", false);
            ("^(linear|polynomial|real)$", "polynomial", true); ("^(linear|polynomial|real)$", "cubic", false);
            ("^(?:ab)+$", "ababab", true); ("^(?:ab)+$", "aba", false); ("^a{2,3}$", "aa", true); ("^a{2,3}$", "aaaa", false);
            ("^a{2,}$", "aaaaa", true); ("^a{2,}$", "a", false); ("^(a?){3}$", "", true); ("^(a*)*$", "aaa", true); ("^(a*)*b$", "aaac", false);
            ("^\\w+\\.\\w+$", "discrete.prime", true); ("^\\w+\\.\\w+$", "discrete", false); ("^[\\w.-]+$", "a_b.c-d", true);
            ("^\\s*$", " \t\n", true); ("^P(0[0-9]|1[0-8])-(0[1-9]|10)-(0[1-9]|1[0-9]|2[0-5])$", "P18-10-25", true);
            ("^P(0[0-9]|1[0-8])-(0[1-9]|10)-(0[1-9]|1[0-9]|2[0-5])$", "P19-01-01", false); ("^[a-z][a-z0-9]*(-[a-z0-9]+)*$", "prove-even", true);
            ("^[a-z][a-z0-9]*(-[a-z0-9]+)*$", "prove--even", false); ("", "anything", true); ("a|", "b", true); ("^$", "", true); ("^$", "x", false);
            ("\\$\\^", "a$^b", true); ("^[]a]+$", "]a]", true); ("^[a-]+$", "a-a", true) ];
        List.iter
          (fun bad -> check (Printf.sprintf "%S is refused" bad) (Result.is_error (Json_schema.pattern_matches bad "x")))
          [ "("; ")"; "[a"; "a{2"; "a{3,2}"; "*a"; "a\\"; "\\p{L}"; "(?=a)"; "[z-a]"; "a{9999}"; "\\1" ]) ]

let () =
  run "schema"
    [ ("status", status_tests); ("codes", code_tests); ("capability identifiers", cap_tests); ("json schema", json_schema_tests) ]
