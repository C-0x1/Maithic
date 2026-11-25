(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the formalization pipeline. Lean is stood in for by functions that object to pieces of
   a statement, and the model by lists of answers, so that every path can be taken; the real ones
   meet in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_gateway
open Maithic_formalize
open Testkit

(* ---- stand-ins ---- *)

let intake ?origin text = is_ok "intake" (Formalize_intake.of_text ?origin text)

let candidate ?(id = 1) ?(reading = "") ?(imports = []) signature =
  { Formalize_candidate.id; name = Printf.sprintf "problem_%d" id; signature; imports; reading; provenance = Formalize_candidate.given }

let signature (c : Formalize_candidate.t) = c.Formalize_candidate.signature

(* A Lean that objects to pieces of a statement: where one stands, it says what is given for it;
   a statement it does not object to elaborates, and is printed as a statement with [∀]. *)
let objecting ?(seen = ref []) objections : Formalize_elab.compile =
 fun c ->
  seen := !seen @ [ signature c ];
  match List.find_opt (fun (needle, _) -> Text.contains (signature c) needle) objections with
  | Some (_, said) -> { Formalize_elab.ok = false; statement = ""; said }
  | None -> { Formalize_elab.ok = true; statement = "∀ " ^ signature c; said = "" }

let by_model = { Formalize_candidate.origin = "repair"; provider = "anthropic"; model = "a-model"; prompt = "formalize.repair"; round = 0 }

(* A model that repairs with the answers it is given, in turn, and notes what it was shown. *)
let answering answers asked : Formalize_elab.repair =
  let left = ref answers in
  fun ~candidate ~error ~said ~failures ->
    asked := !asked @ [ (signature candidate, Formalize_error.name error, said, failures) ];
    match !left with
    | [] -> Error "no answer is left"
    | answer :: rest ->
      left := rest;
      Result.map (fun text -> (text, by_model)) answer

let sound = { Formalize_faith.unused = []; traps = []; annotated = "" }

(* A Lean for the checks: each answer depends on the statement alone. *)
let lean ?(vacuous = fun _ -> Ok None) ?(refute = fun _ -> Ok (Formalize_faith.Survived "300 sampled cases")) ?(closes = fun _ _ -> None)
    ?(inspect = fun _ -> Ok sound) () =
  { Formalize_faith.inspect; vacuous; refute; closes }

(* A model for the round trip, which notes every statement it is shown and every pair it is asked to compare. *)
let reading ?(back = fun ~statement -> Ok ("It says: " ^ statement)) ?(judge = fun ~problem:_ ~english:_ -> Ok (10, [])) ?(shown = ref []) ?(compared = ref []) () =
  { Formalize_faith.back =
      (fun ~statement ->
        shown := !shown @ [ statement ];
        back ~statement);
    judge =
      (fun ~problem ~english ->
        compared := !compared @ [ (problem, english) ];
        judge ~problem ~english) }

let check_of (f : Formalize_faith.t) name =
  match List.find_opt (fun (c : Formalize_faith.check) -> c.Formalize_faith.name = name) f.Formalize_faith.checks with
  | Some c -> c
  | None -> fail ("no check called " ^ name)

let outcome f name = (check_of f name).Formalize_faith.outcome
let note f name = (check_of f name).Formalize_faith.note
let outcomes (f : Formalize_faith.t) = List.map (fun (c : Formalize_faith.check) -> c.Formalize_faith.name ^ " " ^ c.Formalize_faith.outcome) f.Formalize_faith.checks

let scripted answers =
  let handler, seen = Scripted.sequence (List.map (fun text -> Ok (Scripted.answer text)) answers) in
  match Gateway.create ~scripted:handler (Maithic_config.Config.of_defaults ()) with Ok g -> (g, seen) | Error f -> fail (Fault.to_string f)

let user_text (request : Gateway_types.request) = String.concat "\n" (List.map (fun (m : Gateway_types.message) -> m.Gateway_types.content) request.Gateway_types.messages)

(* The tools of a run: the statements are given, and everything else is a stand-in. *)
let tools ?(compile = objecting []) ?repair ?(lean = lean ()) ?model ?(same = fun _ _ -> None) statements =
  { Formalize_run.draft =
      (fun ~count _ -> Result.map (fun given -> List.filteri (fun i _ -> i < count) (Formalize_run.given ~name:"problem" ~imports:[] given)) statements);
    compile; repair; lean; model; same }

let verdicts (run : Formalize_run.t) = List.map (fun (e : Formalize_run.entry) -> Formalize_run.verdict_name e.Formalize_run.verdict) run.Formalize_run.entries
let squares = intake "Show that the square of a negative integer is positive."

(* ---- intake ---- *)

let ingest_tests =
  [ case "P02-01-01 a problem is read from the argument, from a file or from standard input" (fun () ->
        let from_argument = is_ok "argument" (Formalize_intake.read ~argument:(Some "Show that 2 + 2 = 4.") ~file:None ()) in
        equal_string "the text" "Show that 2 + 2 = 4." from_argument.Formalize_intake.text;
        equal_string "where it came from" "argument" from_argument.Formalize_intake.origin;
        let path = Filename.temp_file "maithic-problem" ".txt" in
        is_ok "write" (Fs.write path "Prove that n + 0 = n.\n");
        let from_file = is_ok "file" (Formalize_intake.read ~argument:None ~file:(Some path) ()) in
        Sys.remove path;
        equal_string "the text of the file, without the blank space at its end" "Prove that n + 0 = n." from_file.Formalize_intake.text;
        equal_string "the text as it was given is kept" "Prove that n + 0 = n.\n" from_file.Formalize_intake.source;
        equal_string "the file is where it came from" path from_file.Formalize_intake.origin;
        let typed = fun () -> "Find all n with n * n = n.\n" in
        let from_dash = is_ok "dash" (Formalize_intake.read ~stdin:typed ~argument:None ~file:(Some "-") ()) in
        equal_string "a dash is standard input" "stdin" from_dash.Formalize_intake.origin;
        let from_nothing = is_ok "nothing" (Formalize_intake.read ~stdin:typed ~argument:None ~file:None ()) in
        equal_string "and so is giving neither" "Find all n with n * n = n." from_nothing.Formalize_intake.text;
        equal_string "a problem given twice is refused" "give the problem once: as an argument, or as a file"
          (is_error "both" (Formalize_intake.read ~argument:(Some "x") ~file:(Some path) ()));
        check "a file that is not there is an error" (Result.is_error (Formalize_intake.read ~argument:None ~file:(Some path) ())));
    case "P02-01-01 text that is not UTF-8, and text that holds nothing, are refused" (fun () ->
        equal_string "not UTF-8" "the problem is not valid UTF-8 text" (is_error "bytes" (Formalize_intake.of_text "caf\xE9 = 1"));
        equal_string "nothing" "the problem is empty" (is_error "empty" (Formalize_intake.of_text ""));
        equal_string "blank space" "the problem is empty" (is_error "blank" (Formalize_intake.of_text " \n\t "));
        let accented = intake "Montrer que x\xC2\xB2 \xE2\x89\xA5 0 pour tout entier x, d'apr\xC3\xA8s Poincar\xC3\xA9." in
        contains "text in any language is taken" accented.Formalize_intake.text "x^2 \xE2\x89\xA5 0";
        equal_strings "and an accent is not mathematics" [ "x^2 \xE2\x89\xA5 0" ] (Formalize_intake.of_kind accented Formalize_intake.Formula));
    case "P02-01-02 operators, superscripts, subscripts and Greek letters get one spelling" (fun () ->
        let same given written = equal_string given written (Formalize_normalize.text given) in
        same "x\xC2\xB2 \xE2\x88\x92 3x \xE2\x89\xA6 4 \xE2\x87\x92 x \xE2\x89\xA0 2" "x^2 - 3x \xE2\x89\xA4 4 \xE2\x86\x92 x \xE2\x89\xA0 2";
        same "a <= b, b >= c, c != d" "a \xE2\x89\xA4 b, b \xE2\x89\xA5 c, c \xE2\x89\xA0 d";
        same "p <=> q, p => q, p -> q, p <-> q" "p \xE2\x86\x94 q, p \xE2\x86\x92 q, p \xE2\x86\x92 q, p \xE2\x86\x94 q";
        same "x\xE2\x82\x81 + x\xE2\x82\x82 = x\xE2\x82\x81\xE2\x82\x80" "x_1 + x_2 = x_10";
        same "2\xC2\xB9\xE2\x81\xB0 \xC3\x97 3 \xC2\xB7 5 \xC3\xB7 7" "2^10 * 3 * 5 / 7";
        same "x\xE2\x81\xBF\xE2\x81\xBA\xC2\xB9" "x^(n+1)";
        same "\xCF\xB5 < \xCF\x95" "\xCE\xB5 < \xCF\x86";
        same "a\xC2\xA0+\xE2\x80\x89b" "a + b";
        check "the table of symbols can be read" (List.assoc "<=" Formalize_normalize.symbols = "\xE2\x89\xA4"));
    case "P02-01-02 the commands of LaTeX are rewritten, and its dollar signs and the marks of Markdown are taken off" (fun () ->
        let same given written = equal_string given written (Formalize_normalize.text given) in
        same {|$\frac{a+1}{2} \leq b$|} "(a+1)/(2) \xE2\x89\xA4 b";
        same {|$$x^{10} + y_{n+1} \neq 0$$|} "x^10 + y_(n+1) \xE2\x89\xA0 0";
        same {|\alpha \cdot \beta \in \mathbb{N}|} "\xCE\xB1 * \xCE\xB2 \xE2\x88\x88 \xE2\x84\x95";
        same {|\forall n \geq 1, \exists m \ge n|} "\xE2\x88\x80 n \xE2\x89\xA5 1, \xE2\x88\x83 m \xE2\x89\xA5 n";
        same {|\text{gcd}(a, b) \mid a|} "gcd(a, b) \xE2\x88\xA3 a";
        same "## Exercise 3\n\n**Claim.** For `n` odd, $n^2$ is odd." "Exercise 3\n\nClaim. For n odd, n^2 is odd.";
        same "> Let $x \\le y$." "Let x \xE2\x89\xA4 y.";
        same {|a command nobody knows, \foo, is left as it is|} {|a command nobody knows, \foo, is left as it is|};
        check "the table of commands can be read" (List.assoc "leq" Formalize_normalize.commands = "\xE2\x89\xA4"));
    case "P02-01-02 every change is recorded with its place, and a text in one spelling is left as it is" (fun () ->
        let source = {|If $a \leq b$ then a² ≤ b².|} in
        let n = Formalize_normalize.normalize source in
        equal_string "the text" "If a \xE2\x89\xA4 b then a^2 \xE2\x89\xA4 b^2." n.Formalize_normalize.text;
        List.iter
          (fun (c : Formalize_normalize.change) ->
            equal_string "what stood there is what the text holds at that place" c.Formalize_normalize.given
              (String.sub source c.Formalize_normalize.start (c.Formalize_normalize.stop - c.Formalize_normalize.start)))
          n.Formalize_normalize.changes;
        equal_strings "the changes, in the order of the text" [ "$ -> "; {|\leq -> |} ^ "\xE2\x89\xA4"; "$ -> "; "\xC2\xB2 -> ^2"; "\xC2\xB2 -> ^2" ]
          (List.map (fun (c : Formalize_normalize.change) -> c.Formalize_normalize.given ^ " -> " ^ c.Formalize_normalize.written) n.Formalize_normalize.changes);
        (* A place in the normalized text leads back to the place it came from. *)
        let at = match Text.find_sub n.Formalize_normalize.text "then" with Some i -> i | None -> fail "no then" in
        equal_string "a word that was copied" "then" (String.sub source (Formalize_normalize.source_offset n at) 4);
        let power = match Text.find_sub n.Formalize_normalize.text "^2" with Some i -> i | None -> fail "no power" in
        equal_string "a character that was rewritten" "\xC2\xB2"
          (String.sub source (Formalize_normalize.source_offset n power) (Formalize_normalize.source_end n (power + 1) - Formalize_normalize.source_offset n power)));
    property ~count:300 ~show:String.escaped "P02-01-02 normalizing a normalized text changes nothing"
      (Gen.map (String.concat " ")
         (Gen.list ~max:12
            (Gen.one_of
               [ "x\xC2\xB2"; "<="; ">="; "!="; "<=>"; "=>"; "->"; {|\leq|}; {|\frac{a}{b}|}; {|\mathbb{R}|}; "$x$"; "$$"; "x^{n+1}"; "y_{2}"; "\xE2\x88\x92"; "\xC3\x97"; "a"; "and";
                 "\xCF\xB5"; "**so**"; "`n`"; "("; ")"; "12.5"; {|\alpha|}; "\xE2\x82\x81"; "\n"; "#"; "n\xE2\x81\xBF"; "\xE2\x89\xA6"; {|\foo|} ])))
      (fun source ->
        let once = Formalize_normalize.text source in
        Formalize_normalize.text once = once);
    case "P02-01-03 the record holds every element with its kind and its place in the text as given" (fun () ->
        let source = {|Prove that $n^{2} \geq n$ for every natural number $n$.|} in
        let record = intake ~origin:"problems/square.md" source in
        equal_string "the text in one spelling" "Prove that n^2 \xE2\x89\xA5 n for every natural number n." record.Formalize_intake.text;
        equal_strings "the elements, in the order of the text"
          [ "task Prove that"; "formula n^2 \xE2\x89\xA5 n"; "symbol n"; "number 2"; "symbol n"; "quantifier for every"; "constraint natural number"; "symbol n" ]
          (List.map (fun (e : Formalize_intake.element) -> Formalize_intake.kind_name e.Formalize_intake.kind ^ " " ^ e.Formalize_intake.text) record.Formalize_intake.elements);
        let stretch (e : Formalize_intake.element) = String.sub source e.Formalize_intake.start (e.Formalize_intake.stop - e.Formalize_intake.start) in
        let of_kind kind = List.filter (fun (e : Formalize_intake.element) -> e.Formalize_intake.kind = kind) record.Formalize_intake.elements in
        equal_strings "the formula, as it was written" [ {|n^{2} \geq n|} ] (List.map stretch (of_kind Formalize_intake.Formula));
        equal_strings "the number, as it was written" [ "^{2}" ] (List.map stretch (of_kind Formalize_intake.Number));
        equal_strings "what is asked" [ "Prove that" ] (List.map stretch (of_kind Formalize_intake.Task));
        (* Whatever the element, the stretch it points at is where it came from. *)
        List.iter
          (fun text ->
            let r = intake text in
            check "there are elements" (r.Formalize_intake.elements <> []);
            List.iter
              (fun (e : Formalize_intake.element) ->
                check "a place inside the text" (e.Formalize_intake.start >= 0 && e.Formalize_intake.stop > e.Formalize_intake.start && e.Formalize_intake.stop <= String.length text);
                contains "the stretch holds the element"
                  (Formalize_normalize.text (String.sub text e.Formalize_intake.start (e.Formalize_intake.stop - e.Formalize_intake.start)))
                  e.Formalize_intake.text)
              r.Formalize_intake.elements)
          [ source; {|Find all real numbers $x$ such that $\frac{x+1}{2} \leq 3$ and $x^{2} \neq 4$.|}; "Show that x\xE2\x82\x81\xC2\xB2 + x\xE2\x82\x82\xC2\xB2 \xE2\x89\xA5 2x\xE2\x82\x81x\xE2\x82\x82 for all integers.";
            "## Exercise 3\n\nFor natural numbers $a$ and $b$, show that $(a - b) + b = a$." ];
        let j = Formalize_intake.to_json record in
        equal_string "where it came from, in the record" "problems/square.md" (Json.string_or "" "origin" j);
        equal_string "the text as given, in the record" source (Json.string_or "" "source" j);
        equal_int "every element, in the record" 8 (List.length (Json.list_or_empty "elements" j));
        (match Json.list_or_empty "elements" j with
         | first :: _ ->
           equal_string "its kind" "task" (Json.string_or "" "kind" first);
           equal_int "its first byte" 0 (Json.int_or (-1) "start" first);
           equal_int "the byte after its last" 10 (Json.int_or (-1) "stop" first)
         | [] -> fail "no element"));
    case "P02-01-03 numbers, symbols and what is asked are read off the record" (fun () ->
        let labelled = intake "Problem 12. Show that 7 divides 21, as in Exercise 3.2, for x_1 = 7 and y \xE2\x89\xA5 2." in
        equal_strings "the numbers of the problem, without labels and indices" [ "7"; "21"; "2" ] (Formalize_intake.numbers labelled);
        let articles = intake "Let a be a positive integer. Show that a divides a^2, and I think a = 1 is a special case, i.e. the easy one." in
        equal_strings "a letter that stands for a variable, each once" [ "a" ] (Formalize_intake.symbols articles);
        equal_strings "the formulas" [ "a^2"; "a = 1" ] (Formalize_intake.of_kind articles Formalize_intake.Formula);
        equal_strings "the words that constrain" [ "positive"; "integer"; "divides" ] (Formalize_intake.of_kind articles Formalize_intake.Constraint);
        let prose = intake "A non-negative number on the x-axis is well-defined." in
        equal_strings "a hyphen is not a minus" [] (Formalize_intake.of_kind prose Formalize_intake.Formula);
        equal_strings "the longer phrase is taken" [ "non-negative" ] (Formalize_intake.of_kind prose Formalize_intake.Constraint);
        equal_string "a proof is asked for" "prove" (Formalize_intake.task labelled);
        let search = intake "Find all real numbers x such that x^2 = 4." in
        equal_string "a value is asked for" "find" (Formalize_intake.task search);
        equal_strings "what is asked is not a quantifier" [] (Formalize_intake.of_kind search Formalize_intake.Quantifier);
        equal_string "nothing asked means a proof" "prove" (Formalize_intake.task (intake "n + 0 = n"));
        equal_string "the frame, a line for each kind that occurs"
          "task: find\nformulas: x^2 = 4\nsymbols: x\nnumbers: 2; 4\nconstraints: real numbers" (Formalize_intake.frame search)) ]

(* ---- drafting ---- *)

let draft_answer =
  {|Here are the statements.
{"candidates": [
  {"statement": "(n : Int) (h : n < 0) : 0 < n ^ 2", "reading": "An integer below zero."},
  {"statement": "theorem sq_pos (n : Int)  (h : n < 0) :\n  0 < n ^ 2 := by sorry", "reading": "The same, written as a theorem."},
  {"statement": "", "reading": "Nothing."},
  {"statement": "(n : Nat) (h : n < 0) : 0 < n ^ 2", "reading": " A natural number. "},
  "(n : Int) (h : n ≤ 0) : 0 < n ^ 2",
  {"statement": "(n : Int) : n < 0 → 0 < n ^ 2", "reading": "With an arrow."}
]}|}

let draft_tests =
  [ case "P02-02-01 the drafting prompt carries the problem, its frame and how many statements are wanted" (fun () ->
        let bindings = Formalize_draft.bindings ~count:4 ~imports:[] squares in
        equal_string "the problem" "Show that the square of a negative integer is positive." (List.assoc "problem" bindings);
        equal_string "what a first reading found" "task: prove\nconstraints: negative; integer; positive" (List.assoc "frame" bindings);
        equal_string "how many" "4" (List.assoc "count" bindings);
        equal_string "what a statement may use" "the Lean core library, without Mathlib" (List.assoc "libraries" bindings);
        equal_string "and with imports" "the Lean core library and the modules Mathlib.Tactic, Std"
          (List.assoc "libraries" (Formalize_draft.bindings ~count:1 ~imports:[ "Mathlib.Tactic"; "Std" ] squares));
        (* The prompt that ships takes exactly these values. *)
        let prompt = is_ok "load" (Prompt.load Formalize_draft.prompt) in
        equal_strings "its placeholders" [ "count"; "frame"; "libraries"; "problem" ] (List.sort compare (Prompt.placeholders prompt));
        let system, user = is_ok "render" (Prompt.render prompt bindings) in
        contains "statements, not proofs" system "You write statements, not proofs";
        contains "several readings" system "in 4 different ways";
        contains "what a statement is" system "No name, no `:=`, no proof";
        contains "the traps are named" system "subtraction of natural numbers stops at zero";
        contains "the answer" system "\"candidates\"";
        contains "the problem is data" user "<data>\nShow that the square of a negative integer is positive.\n</data>";
        contains "and so is the frame" user "constraints: negative; integer; positive";
        check "it is one of the prompts that ship" (List.mem "formalize.draft" Prompt.names));
    case "P02-02-01 the model is asked through the gateway, and what it drafts is a theorem with sorry for a proof" (fun () ->
        let gateway, seen = scripted [ draft_answer ] in
        let drafted = is_ok "ask" (Formalize_draft.ask gateway ~max_tokens:700 ~name:"problem" ~count:3 ~imports:[] squares) in
        equal_int "as many as were asked for" 3 (List.length drafted);
        (match seen () with
         | [ (request : Gateway_types.request) ] ->
           equal_string "the purpose of the call" "formalize.draft" request.Gateway_types.purpose;
           equal_int "the limit on the answer" 700 request.Gateway_types.max_tokens;
           check "the answer has a schema" (match request.Gateway_types.schema with Some ("statement_candidates", _) -> true | _ -> false);
           contains "the problem is in the request" (user_text request) "the square of a negative integer"
         | _ -> fail "one request was expected");
        let first = List.hd drafted in
        equal_string "a draft as a theorem" "set_option autoImplicit false in\ntheorem problem_1 (n : Int) (h : n < 0) : 0 < n ^ 2 := sorry\n" (Formalize_candidate.theorem first);
        equal_string "who drafted it" "draft" first.Formalize_candidate.provenance.Formalize_candidate.origin;
        equal_string "with which prompt" "formalize.draft" first.Formalize_candidate.provenance.Formalize_candidate.prompt;
        check "and which model" (first.Formalize_candidate.provenance.Formalize_candidate.model <> "");
        (* A model that cannot be asked is an error with its reason, not an empty list. *)
        let silent, _ = scripted [] in
        check "no answer is an error" (Result.is_error (Formalize_draft.ask silent ~max_tokens:700 ~name:"problem" ~count:3 ~imports:[] squares)));
    case "P02-02-02 several statements are read from one answer, each once" (fun () ->
        let read count = Formalize_candidate.of_answer ~name:"problem" ~imports:[] ~provenance:by_model ~count draft_answer in
        equal_strings "distinct statements, in the order of the answer"
          [ "(n : Int) (h : n < 0) : 0 < n ^ 2"; "(n : Nat) (h : n < 0) : 0 < n ^ 2"; "(n : Int) (h : n ≤ 0) : 0 < n ^ 2"; "(n : Int) : n < 0 → 0 < n ^ 2" ]
          (List.map signature (read 8));
        equal_ints "numbered from one" [ 1; 2; 3; 4 ] (List.map (fun (c : Formalize_candidate.t) -> c.Formalize_candidate.id) (read 8));
        equal_strings "each with a name of its own" [ "problem_1"; "problem_2"; "problem_3"; "problem_4" ]
          (List.map (fun (c : Formalize_candidate.t) -> c.Formalize_candidate.name) (read 8));
        equal_strings "with the reading the model gave" [ "An integer below zero."; "A natural number."; ""; "With an arrow." ]
          (List.map (fun (c : Formalize_candidate.t) -> c.Formalize_candidate.reading) (read 8));
        equal_int "no more than were asked for" 2 (List.length (read 2));
        equal_int "an answer without statements is an empty list" 0
          (List.length (Formalize_candidate.of_answer ~name:"p" ~imports:[] ~provenance:by_model ~count:3 "I cannot state this."));
        equal_int "and so is one that is not what was asked for" 0
          (List.length (Formalize_candidate.of_answer ~name:"p" ~imports:[] ~provenance:by_model ~count:3 {|{"candidates": "none"}|})));
    case "P02-02-02 of two statements that Lean proves to say the same, the later is dropped before a model reads it" (fun () ->
        let shown = ref [] in
        let same (a : Formalize_candidate.t) (b : Formalize_candidate.t) =
          if Text.contains (signature a) "0 < n ^ 2" && Text.contains (signature b) "n ^ 2 > 0" then Some "definitional" else None
        in
        let run =
          is_ok "run"
            (Formalize_run.run
               (tools ~same ~model:(reading ~shown ())
                  (Ok [ "(n : Int) (h : n < 0) : 0 < n ^ 2"; "(n : Int) (h : n < 0) : n ^ 2 > 0"; "(n : Int) (h : n < 0) : 0 < n * n" ]))
               squares)
        in
        equal_strings "what became of each" [ "chosen"; "duplicate"; "kept" ] (verdicts run);
        (match (List.nth run.Formalize_run.entries 1).Formalize_run.verdict with
         | Formalize_run.Duplicate (of_, relation) ->
           equal_int "the statement it repeats" 1 of_;
           equal_string "how Lean related them" "definitional" relation
         | _ -> fail "a duplicate was expected");
        check "no check was made on the duplicate" ((List.nth run.Formalize_run.entries 1).Formalize_run.faith = None);
        equal_strings "and no model read it" [ "∀ (n : Int) (h : n < 0) : 0 < n ^ 2"; "∀ (n : Int) (h : n < 0) : 0 < n * n" ] !shown);
    case "P02-02-03 a candidate is a signature with its reading and where it came from" (fun () ->
        let reduced given expected = equal_string given expected (Formalize_candidate.signature_of given) in
        reduced "(a b : Nat) : a + b = b + a" "(a b : Nat) : a + b = b + a";
        reduced "theorem add_comm' (a b : Nat) : a + b = b + a := by sorry" "(a b : Nat) : a + b = b + a";
        reduced "lemma foo : 1 = 1 := sorry" ": 1 = 1";
        reduced "theorem t (a : Nat) : a = a :=\n  by\n  sorry" "(a : Nat) : a = a";
        reduced "```lean\n(a : Nat) : a = a\n```" "(a : Nat) : a = a";
        reduced "  (a : Nat) (h : let b := a; b = a) : a = a  " "(a : Nat) (h : let b := a; b = a) : a = a";
        let c = { (candidate ~id:2 ~reading:"Over the integers." ~imports:[ "Std"; "Mathlib.Tactic" ] "(n : Int)\n    (h : n < 0) :\n  0 < n ^ 2") with Formalize_candidate.provenance = by_model } in
        equal_string "the key ignores how the text is laid out" "(n : Int) (h : n < 0) : 0 < n ^ 2" (Formalize_candidate.key c);
        equal_string "the imports, a line each" "import Std\nimport Mathlib.Tactic\n" (Formalize_candidate.header c);
        equal_string "a proof on several lines is set under the statement"
          "set_option autoImplicit false in\ntheorem problem_2 (n : Int)\n    (h : n < 0) :\n  0 < n ^ 2 :=\n  by\n    simp\n    omega\n"
          (Formalize_candidate.theorem ~proof:"by\n  simp\n  omega" c);
        let j = Formalize_candidate.to_json c in
        equal_int "its number" 2 (Json.int_or 0 "id" j);
        equal_string "its name" "problem_2" (Json.string_or "" "name" j);
        equal_string "its reading" "Over the integers." (Json.string_or "" "reading" j);
        equal_strings "its imports" [ "Std"; "Mathlib.Tactic" ] (Json.strings (Json.List (Json.list_or_empty "imports" j)));
        (match Json.member "provenance" j with
         | Some p ->
           equal_string "who wrote it" "repair" (Json.string_or "" "origin" p);
           equal_string "the provider" "anthropic" (Json.string_or "" "provider" p);
           equal_string "the prompt" "formalize.repair" (Json.string_or "" "prompt" p)
         | None -> fail "no provenance");
        (* A statement a person wrote has no model behind it. *)
        (match Json.member "provenance" (Formalize_candidate.to_json (candidate "(a : Nat) : a = a")) with
         | Some p ->
           equal_string "given" "given" (Json.string_or "" "origin" p);
           check "no provider is named" (Json.member "provider" p = None)
         | None -> fail "no provenance");
        equal_strings "statements a person wrote, each once"
          [ "(a : Nat) : a = a"; "(a : Int) : a = a" ]
          (List.map signature (Formalize_run.given ~name:"t" ~imports:[] [ "(a : Nat) : a = a"; "(a : Nat)  :  a = a"; ""; "theorem u (a : Int) : a = a := sorry" ]))) ]

(* ---- elaboration and repair ---- *)

let unknown_even = "Unknown identifier `Even`\n\nNote: It is not possible to treat `Even` as an implicitly bound variable here because the `autoImplicit` option is set to `false`."

let elab_tests =
  [ case "P02-03-01 each candidate is compiled, and what Lean says is collected" (fun () ->
        let seen = ref [] in
        let compile = objecting ~seen [ ("Even", unknown_even) ] in
        let good = Formalize_elab.run ~compile (candidate "(a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0") in
        check "a statement Lean accepts elaborates" good.Formalize_elab.elaborated;
        equal_string "and is kept as Lean prints it" "∀ (a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0" good.Formalize_elab.statement;
        check "at the first try" (Formalize_elab.first_try good);
        equal_string "nothing to note" "" good.Formalize_elab.note;
        let bad = Formalize_elab.run ~compile (candidate "(a : Nat) (h : Even a) : Even (a + a)") in
        check "a statement Lean rejects does not" (not bad.Formalize_elab.elaborated);
        equal_string "no statement is kept for it" "" bad.Formalize_elab.statement;
        (match bad.Formalize_elab.history with
         | [ attempt ] ->
           equal_string "what Lean said" unknown_even attempt.Formalize_elab.said;
           equal_string "and its class" "unknown-constant" attempt.Formalize_elab.error_class
         | _ -> fail "one attempt was expected");
        equal_strings "each statement went to Lean once" [ "(a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0"; "(a : Nat) (h : Even a) : Even (a + a)" ] !seen);
    case "P02-03-02 what Lean says about a statement is put in a class" (fun () ->
        let class_of said = Formalize_error.name (Formalize_error.classify said) in
        equal_string "a name Lean does not know" "unknown-constant" (class_of unknown_even);
        equal_string "a constant Lean does not know" "unknown-constant" (class_of "Unknown constant `Nat.add_commutes`");
        equal_string "a module that is not there" "unknown-module" (class_of "unknown module prefix 'Mathlib'\n\nNo directory 'Mathlib' or file 'Mathlib.olean' in the search path entries");
        equal_string "an instance Lean cannot find" "failed-instance" (class_of "failed to synthesize\n  HDiv String Nat ?m.5\n\nHint: Additional diagnostic information may be available");
        equal_string "two kinds of number" "type-mismatch" (class_of "Type mismatch\n  n\nhas type\n  Nat\nbut is expected to have type\n  Int");
        equal_string "an application of the wrong type" "type-mismatch" (class_of "Application type mismatch: The argument\n  h\nhas type\n  a < b\nbut is expected to have type\n  b < a");
        equal_string "a notation with several readings" "ambiguous" (class_of "ambiguous, possible interpretations \n  a ∣ b : Prop\n  \n  a | b : Nat");
        equal_string "text that is not Lean" "syntax" (class_of "unexpected token ':='; expected term");
        equal_string "a character Lean does not read" "syntax" (class_of "expected token");
        equal_string "anything else" "other" (class_of "maximum recursion depth has been reached");
        equal_string "of several errors the first decides" "syntax" (class_of ("expected token\n\n" ^ unknown_even));
        (match Formalize_error.classify unknown_even with
         | Formalize_error.Unknown_constant name -> equal_string "the name that is not known" "Even" name
         | _ -> fail "an unknown constant was expected");
        equal_strings "the classes" [ "unknown-constant"; "unknown-module"; "failed-instance"; "type-mismatch"; "ambiguous"; "syntax"; "other" ] Formalize_error.names;
        List.iter
          (fun error ->
            check (Formalize_error.name error ^ " is one of the classes") (List.mem (Formalize_error.name error) Formalize_error.names);
            check (Formalize_error.name error ^ " says what a repair has to do") (String.length (Formalize_error.hint error) > 20))
          [ Formalize_error.Unknown_constant "x"; Formalize_error.Unknown_module "M"; Formalize_error.Failed_instance; Formalize_error.Type_mismatch; Formalize_error.Ambiguous;
            Formalize_error.Syntax; Formalize_error.Other ]);
    case "P02-03-03 the repair prompt shows the statement, the error with its class and the signatures of the names Lean knows" (fun () ->
        let statement = "(a b : Nat) (ha : Even a) (hb : ∃ k, b = Nat.succ k) : Even (a + List.length [a, b])" in
        equal_strings "the names a statement binds" [ "a"; "b"; "ha"; "hb"; "k" ] (Formalize_repair.bound statement);
        equal_strings "the names it takes from a library" [ "Nat"; "Even"; "Nat.succ"; "List.length" ] (Formalize_repair.identifiers statement);
        equal_strings "bound by a quantifier or a function" [ "x"; "y"; "z" ] (Formalize_repair.bound "(h : ∀ x y : Nat, x = y) : ∃ z, (fun w => w) z = z" |> List.filter (fun n -> n <> "h" && n <> "w"));
        let bindings =
          Formalize_repair.bindings ~imports:[] ~intake:squares ~candidate:(candidate statement) ~error:(Formalize_error.classify unknown_even) ~said:unknown_even
            ~signatures:[ ("Nat.succ", "Nat.succ (n : Nat) : Nat"); ("Nat", "Nat : Type") ]
            ~failures:[ ("(a : Nat) (h : IsEven a) : IsEven a", "Unknown identifier `IsEven`") ]
        in
        equal_string "the statement" statement (List.assoc "statement" bindings);
        equal_string "the class, with what a repair of it has to do"
          "unknown-constant. To repair it: replace the name by what it stands for, written out with what the library provides." (List.assoc "class" bindings);
        equal_string "what Lean said" unknown_even (List.assoc "errors" bindings);
        equal_string "the signatures, a line each" "Nat.succ (n : Nat) : Nat\nNat : Type" (List.assoc "signatures" bindings);
        equal_string "what failed before" "(a : Nat) (h : IsEven a) : IsEven a\n-- Lean: Unknown identifier `IsEven`" (List.assoc "attempts" bindings);
        equal_string "the problem is there to keep the meaning" "Show that the square of a negative integer is positive." (List.assoc "problem" bindings);
        equal_string "no signature" "(none)" (Formalize_repair.signatures_text []);
        (* The prompt that ships takes exactly these values, and the model is asked through the gateway. *)
        let prompt = is_ok "load" (Prompt.load Formalize_repair.prompt) in
        equal_strings "its placeholders" [ "attempts"; "class"; "errors"; "libraries"; "problem"; "signatures"; "statement" ] (List.sort compare (Prompt.placeholders prompt));
        let system, _ = is_ok "render" (Prompt.render prompt bindings) in
        contains "the meaning is not the model's to change" system "Do not change what the statement claims";
        let gateway, seen = scripted [ {|{"statement": "theorem t (a b : Nat) (ha : a % 2 = 0) : (a + b) % 2 = b % 2 := sorry"}|} ] in
        let looked_up = ref [] in
        let lookup names =
          looked_up := names;
          [ ("Nat.succ", "Nat.succ (n : Nat) : Nat") ]
        in
        let repair = Formalize_repair.ask gateway ~max_tokens:600 ~lookup ~imports:[] ~intake:squares in
        let repaired, provenance = is_ok "ask" (repair ~candidate:(candidate statement) ~error:(Formalize_error.classify unknown_even) ~said:unknown_even ~failures:[]) in
        equal_string "the repaired statement, as a signature" "(a b : Nat) (ha : a % 2 = 0) : (a + b) % 2 = b % 2" repaired;
        equal_string "who repaired it" "formalize.repair" provenance.Formalize_candidate.prompt;
        equal_strings "the names of the statement were looked up" [ "Nat"; "Even"; "Nat.succ"; "List.length" ] !looked_up;
        (match seen () with
         | [ (request : Gateway_types.request) ] ->
           equal_string "the purpose of the call" "formalize.repair" request.Gateway_types.purpose;
           contains "the signature Lean gave is in the request" (user_text request) "Nat.succ (n : Nat) : Nat";
           contains "and so is the error" (user_text request) "Unknown identifier `Even`"
         | _ -> fail "one request was expected"));
    case "P02-03-04 a statement is repaired within the bound, and the loop ends as soon as Lean accepts one" (fun () ->
        let compile = objecting [ ("IsEven", "Unknown identifier `IsEven`"); ("Even", unknown_even) ] in
        let drafted = candidate "(a : Nat) (h : Even a) : Even (a + a)" in
        let answers = [ Ok "(a : Nat) (h : IsEven a) : IsEven (a + a)"; Ok "(a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0"; Ok "(a : Nat) : a = a" ] in
        let asked = ref [] in
        let fixed = Formalize_elab.run ~compile ~repair:(answering answers asked) drafted in
        check "repaired" fixed.Formalize_elab.elaborated;
        equal_string "the statement Lean accepted" "(a : Nat) (h : a % 2 = 0) : (a + a) % 2 = 0" (signature fixed.Formalize_elab.candidate);
        equal_int "after two repairs" 2 (Formalize_elab.repairs fixed);
        check "and not at the first try" (not (Formalize_elab.first_try fixed));
        equal_int "the model was asked twice, and not a third time" 2 (List.length !asked);
        equal_string "the repaired statement says who wrote it" "repair" fixed.Formalize_elab.candidate.Formalize_candidate.provenance.Formalize_candidate.origin;
        equal_int "and in which round" 2 fixed.Formalize_elab.candidate.Formalize_candidate.provenance.Formalize_candidate.round;
        equal_string "its name and number stay" "problem_1" fixed.Formalize_elab.candidate.Formalize_candidate.name;
        (match !asked with
         | [ (first, first_class, _, before_first); (second, _, said, before_second) ] ->
           equal_string "the first request shows the draft" "(a : Nat) (h : Even a) : Even (a + a)" first;
           equal_string "with the class of its error" "unknown-constant" first_class;
           check "and nothing that failed before it" (before_first = []);
           equal_string "the second shows the repair that failed" "(a : Nat) (h : IsEven a) : IsEven (a + a)" second;
           equal_string "with what Lean said about it" "Unknown identifier `IsEven`" said;
           check "and the statement that failed before, with what Lean said" (before_second = [ ("(a : Nat) (h : Even a) : Even (a + a)", unknown_even) ])
         | _ -> fail "two requests were expected");
        (* One round is not enough for this statement, and none is none. *)
        let asked = ref [] in
        let short = Formalize_elab.run ~rounds:1 ~compile ~repair:(answering answers asked) drafted in
        check "not repaired in one round" (not short.Formalize_elab.elaborated);
        equal_string "and the record says so" "it does not elaborate after 1 repair round(s)" short.Formalize_elab.note;
        equal_int "one request" 1 (List.length !asked);
        let asked = ref [] in
        let none = Formalize_elab.run ~rounds:0 ~compile ~repair:(answering answers asked) drafted in
        equal_string "no round" "it does not elaborate, and no repair was asked for" none.Formalize_elab.note;
        equal_int "no request" 0 (List.length !asked);
        equal_int "the bound when none is given" 2 Formalize_elab.default_rounds;
        (* A statement that elaborates is never shown to the model. *)
        let asked = ref [] in
        ignore (Formalize_elab.run ~compile ~repair:(answering answers asked) (candidate "(a : Nat) : a = a"));
        equal_int "nothing to repair" 0 (List.length !asked));
    case "P02-03-04 the loop also ends when the model has nothing new to offer" (fun () ->
        let compile = objecting [ ("Even", unknown_even) ] in
        let drafted = candidate "(a : Nat) (h : Even a) : Even (a + a)" in
        let ended answers = (Formalize_elab.run ~rounds:5 ~compile ~repair:(answering answers (ref [])) drafted).Formalize_elab.note in
        equal_string "the same statement again" "it does not elaborate, and the model repeated a statement that had failed"
          (ended [ Ok "theorem t (a : Nat)  (h : Even a) : Even (a + a) := sorry" ]);
        equal_string "an earlier one again" "it does not elaborate, and the model repeated a statement that had failed"
          (ended [ Ok "(a : Nat) (h : Even (a + 0)) : Even a"; Ok "(a : Nat) (h : Even a) : Even (a + a)" ]);
        equal_string "no statement" "it does not elaborate, and the model returned no repair" (ended [ Ok "  " ]);
        equal_string "no answer" "it does not elaborate, and the model was not asked for a repair: the provider is down" (ended [ Error "the provider is down" ]);
        equal_string "no model" "it does not elaborate, and there is no model to repair it" (Formalize_elab.run ~compile drafted).Formalize_elab.note);
    case "P02-03-05 the record of an elaboration holds every attempt with what Lean said" (fun () ->
        let compile = objecting [ ("Even", unknown_even); ("²", "expected token") ] in
        let drafted = candidate "(a : Nat) (h : Even a) : Even (a²)" in
        let result = Formalize_elab.run ~compile ~repair:(answering [ Ok "(a : Nat) (h : a % 2 = 0) : a² % 2 = 0"; Ok "(a : Nat) (h : a % 2 = 0) : a ^ 2 % 2 = 0" ] (ref [])) drafted in
        equal_strings "every statement that was compiled, in order"
          [ "0 (a : Nat) (h : Even a) : Even (a²) unknown-constant"; "1 (a : Nat) (h : a % 2 = 0) : a² % 2 = 0 syntax"; "2 (a : Nat) (h : a % 2 = 0) : a ^ 2 % 2 = 0 " ]
          (List.map
             (fun (a : Formalize_elab.attempt) -> Printf.sprintf "%d %s %s" a.Formalize_elab.round a.Formalize_elab.signature a.Formalize_elab.error_class)
             result.Formalize_elab.history);
        let j = Formalize_elab.to_json result in
        equal_string "the status" "elaborated" (Json.string_or "" "status" j);
        check "not at the first try" (Json.member "first_try" j = Some (Json.Bool false));
        equal_int "the repairs" 2 (Json.int_or 0 "repairs" j);
        equal_string "the statement as Lean prints it" "∀ (a : Nat) (h : a % 2 = 0) : a ^ 2 % 2 = 0" (Json.string_or "" "statement" j);
        (match Json.list_or_empty "history" j with
         | [ first; _; last ] ->
           equal_string "the text of the first attempt" "(a : Nat) (h : Even a) : Even (a²)" (Json.string_or "" "statement" first);
           equal_string "its class" "unknown-constant" (Json.string_or "" "error_class" first);
           equal_string "what Lean said" unknown_even (Json.string_or "" "error" first);
           check "the attempt that elaborated has no error" (Json.member "error" last = None && Json.member "elaborated" last = Some (Json.Bool true))
         | _ -> fail "three attempts were expected");
        let failed = Formalize_elab.to_json (Formalize_elab.run ~compile drafted) in
        equal_string "a failure has the status" "failed" (Json.string_or "" "status" failed);
        equal_string "and the reason the loop ended" "it does not elaborate, and there is no model to repair it" (Json.string_or "" "note" failed);
        check "and no statement" (Json.member "statement" failed = None)) ]

(* ---- faithfulness ---- *)

let positive = candidate "(n : Int) (h : n < 0) : 0 < n ^ 2"
let printed = "∀ (n : Int), n < 0 → 0 < n ^ 2"

let faith_tests =
  [ case "P02-04-01 a model that sees the statement, and nothing else, says what it claims" (fun () ->
        let shown = ref [] in
        let model = reading ~back:(fun ~statement:_ -> Ok "For every integer n below zero, the square of n is above zero.") ~shown () in
        let f = Formalize_faith.validate ~model ~lean:(lean ()) squares positive ~statement:printed in
        equal_strings "the model is shown the statement as Lean prints it" [ printed ] !shown;
        equal_string "what it says is kept" "For every integer n below zero, the square of n is above zero." f.Formalize_faith.english;
        equal_string "the check" "passed" (outcome f "back-translation");
        (* The prompt that ships has room for the statement and for nothing of the problem. *)
        let prompt = is_ok "load" (Prompt.load Formalize_back.back_prompt) in
        equal_strings "its one placeholder" [ "statement" ] (Prompt.placeholders prompt);
        let system, user = is_ok "render" (Prompt.render prompt [ ("statement", printed) ]) in
        contains "what is asked" system "say what it does say, literally and completely";
        contains "the statement is data" user ("<data>\n" ^ printed ^ "\n</data>");
        let gateway, seen = scripted [ {|{"english": " For every integer n below zero, n squared is positive. "}|}; {|{"score": 9, "differences": []}|} ] in
        let through = Formalize_faith.validate ~model:(Formalize_back.model gateway ~max_tokens:300) ~lean:(lean ()) squares positive ~statement:printed in
        equal_string "through the gateway" "For every integer n below zero, n squared is positive." through.Formalize_faith.english;
        (match seen () with
         | [ (back : Gateway_types.request); (judge : Gateway_types.request) ] ->
           equal_string "the first call" "formalize.back" back.Gateway_types.purpose;
           check "does not hold the problem" (not (Text.contains (user_text back ^ back.Gateway_types.system) "negative integer"));
           equal_string "the second call" "formalize.judge" judge.Gateway_types.purpose;
           check "does not hold the Lean statement" (not (Text.contains (user_text judge) printed))
         | _ -> fail "two requests were expected");
        equal_string "an answer without the English" "" (Formalize_back.english_of_answer {|{"translation": "x"}|});
        (* Without a model, and without an answer, the check is skipped and says why. *)
        let alone = Formalize_faith.validate ~lean:(lean ()) squares positive ~statement:printed in
        equal_string "no model" "skipped no model was asked" (outcome alone "back-translation" ^ " " ^ note alone "back-translation");
        let down = Formalize_faith.validate ~model:(reading ~back:(fun ~statement:_ -> Error "the provider is down") ()) ~lean:(lean ()) squares positive ~statement:printed in
        equal_string "no answer" "skipped the provider is down" (outcome down "back-translation" ^ " " ^ note down "back-translation");
        equal_string "and nothing to compare" "skipped there is no back-translation to compare" (outcome down "round-trip" ^ " " ^ note down "round-trip"));
    case "P02-04-02 the reading is compared with the problem, and the agreement is scored" (fun () ->
        let compared = ref [] in
        let judged score differences = reading ~back:(fun ~statement:_ -> Ok "In English.") ~judge:(fun ~problem:_ ~english:_ -> Ok (score, differences)) ~compared () in
        let agrees = Formalize_faith.validate ~model:(judged 9 []) ~lean:(lean ()) squares positive ~statement:printed in
        equal_strings "the problem and the reading are what is compared" [ "Show that the square of a negative integer is positive. / In English." ]
          (List.map (fun (problem, english) -> problem ^ " / " ^ english) !compared);
        check "the score is kept" (agrees.Formalize_faith.agreement = Some 9);
        equal_string "nine of ten passes" "passed agreement 9 of 10" (outcome agrees "round-trip" ^ " " ^ note agrees "round-trip");
        check "and is the score of the statement" (abs_float (agrees.Formalize_faith.score -. 0.9) < 1e-9);
        let differs = Formalize_faith.validate ~model:(judged 6 [ "The statement allows zero."; "The problem says integer." ]) ~lean:(lean ()) squares positive ~statement:printed in
        equal_string "six of ten is flagged, with the differences" "flagged agreement 6 of 10: The statement allows zero.; The problem says integer."
          (outcome differs "round-trip" ^ " " ^ note differs "round-trip");
        equal_strings "the differences are kept" [ "The statement allows zero."; "The problem says integer." ] differs.Formalize_faith.differences;
        equal_strings "what flagged it" [ "round-trip" ] (Formalize_faith.flagged differs);
        equal_int "eight is the least that passes" 8 Formalize_faith.agreement_threshold;
        equal_string "eight passes" "passed" (outcome (Formalize_faith.validate ~model:(judged 8 []) ~lean:(lean ()) squares positive ~statement:printed) "round-trip");
        equal_string "seven does not" "flagged" (outcome (Formalize_faith.validate ~model:(judged 7 []) ~lean:(lean ()) squares positive ~statement:printed) "round-trip");
        check "a score beyond the scale is brought back to it"
          ((Formalize_faith.validate ~model:(judged 14 []) ~lean:(lean ()) squares positive ~statement:printed).Formalize_faith.agreement = Some 10);
        (* The answer of a model, read. *)
        check "a score and differences" (Formalize_back.judgement_of_answer {|Sure. {"score": 7, "differences": ["One.", " ", "Two."]}|} = Ok (7, [ "One."; "Two." ]));
        check "a score written with a point" (Formalize_back.judgement_of_answer {|{"score": 8.0, "differences": []}|} = Ok (8, []));
        equal_string "no score" "the model's answer has no score" (is_error "none" (Formalize_back.judgement_of_answer {|{"differences": []}|}));
        equal_string "a score off the scale" "the model's answer has a score that is not a number from 0 to 10"
          (is_error "off" (Formalize_back.judgement_of_answer {|{"score": 11, "differences": []}|}));
        equal_string "no object" "the model's answer is not a JSON object" (is_error "text" (Formalize_back.judgement_of_answer "They agree."));
        (* How the score of a statement is made. *)
        let checks flagged =
          List.map (fun name -> { Formalize_faith.name; outcome = (if List.mem name flagged then "flagged" else "passed"); note = "" }) Formalize_faith.check_names
        in
        let score ?agreement flagged = Formalize_faith.score ~agreement (checks flagged) in
        check "the agreement, as a share" (abs_float (score ~agreement:10 [] -. 1.0) < 1e-9);
        check "the middle when nobody was asked" (abs_float (score [] -. 0.6) < 1e-9);
        check "less for a statement that is trivial" (abs_float (score ~agreement:10 [ "triviality" ] -. 0.7) < 1e-9);
        check "less for a number left out" (abs_float (score ~agreement:10 [ "numbers" ] -. 0.8) < 1e-9);
        check "a little less for a trap" (abs_float (score ~agreement:10 [ "traps" ] -. 0.95) < 1e-9);
        check "never nothing for a statement that was not rejected" (score ~agreement:0 [ "triviality"; "numbers"; "variables"; "traps" ] > 0.0);
        check "nothing for one that was"
          (Formalize_faith.score ~agreement:(Some 10) [ { Formalize_faith.name = "vacuity"; outcome = "failed"; note = "" } ] = 0.0));
    case "P02-04-03 a statement whose hypotheses contradict each other is rejected" (fun () ->
        let shown = ref [] in
        let vacuous _ = Ok (Some "the hypothesis h cannot hold (shown by omega)") in
        let refuted = ref 0 in
        let refute _ =
          incr refuted;
          Ok (Formalize_faith.Survived "")
        in
        let f = Formalize_faith.validate ~model:(reading ~shown ()) ~lean:(lean ~vacuous ~refute ()) squares (candidate "(n : Nat) (h : n < 0) : 0 < n ^ 2") ~statement:"∀ (n : Nat), n < 0 → 0 < n ^ 2" in
        check "rejected, and why" (f.Formalize_faith.rejected = Some "vacuous: the hypothesis h cannot hold (shown by omega)");
        equal_string "the check failed" "failed the hypothesis h cannot hold (shown by omega)" (outcome f "vacuity" ^ " " ^ note f "vacuity");
        check "its score is nothing" (f.Formalize_faith.score = 0.0);
        equal_int "no counterexample was looked for" 0 !refuted;
        equal_strings "and no model read it" [] !shown;
        equal_strings "every other check is skipped"
          [ "vacuity failed"; "counterexample skipped"; "triviality skipped"; "numbers skipped"; "variables skipped"; "traps skipped"; "back-translation skipped"; "round-trip skipped" ]
          (outcomes f);
        (* A check that could not be made says so, and rejects nothing. *)
        let unasked = Formalize_faith.validate ~lean:(lean ~vacuous:(fun _ -> Error "the statement checks are not loaded") ()) squares positive ~statement:printed in
        equal_string "skipped, with the reason" "skipped the statement checks are not loaded" (outcome unasked "vacuity" ^ " " ^ note unasked "vacuity");
        check "not rejected" (unasked.Formalize_faith.rejected = None);
        equal_string "a statement that can hold passes" "passed" (outcome (Formalize_faith.validate ~lean:(lean ()) squares positive ~statement:printed) "vacuity"));
    case "P02-04-04 a statement that trivial, rfl or simp closes is flagged" (fun () ->
        equal_strings "the proofs that are tried" [ "by trivial"; "by rfl"; "by simp" ] Formalize_faith.trivial_proofs;
        let tried = ref [] in
        let closes _ proofs =
          tried := proofs;
          Some "by simp"
        in
        let f = Formalize_faith.validate ~model:(reading ()) ~lean:(lean ~closes ()) squares (candidate "(a b : Int) : a - b + b = a") ~statement:"∀ (a b : Int), a - b + b = a" in
        equal_strings "Lean was given those proofs" Formalize_faith.trivial_proofs !tried;
        equal_string "flagged, with the proof that closed it" "flagged Lean accepts `by simp` for a proof" (outcome f "triviality" ^ " " ^ note f "triviality");
        check "a flag does not reject" (f.Formalize_faith.rejected = None);
        check "and costs part of the score" (abs_float (f.Formalize_faith.score -. 0.7) < 1e-9);
        equal_strings "what flagged it" [ "triviality" ] (Formalize_faith.flagged f);
        equal_string "a statement none of them closes passes" "passed" (outcome (Formalize_faith.validate ~lean:(lean ()) squares positive ~statement:printed) "triviality"));
    case "P02-04-05 a statement with a counterexample that Lean confirmed is discarded" (fun () ->
        let shown = ref [] in
        let refute _ = Ok (Formalize_faith.Refuted [ ("a", "0"); ("b", "1") ]) in
        let f = Formalize_faith.validate ~model:(reading ~shown ()) ~lean:(lean ~refute ()) squares (candidate "(a b : Nat) : a - b + b = a") ~statement:"∀ (a b : Nat), a - b + b = a" in
        check "rejected, with the point" (f.Formalize_faith.rejected = Some "refuted: it fails at a = 0, b = 1");
        equal_string "the check failed" "failed it fails at a = 0, b = 1" (outcome f "counterexample" ^ " " ^ note f "counterexample");
        equal_strings "no model read it" [] !shown;
        check "its score is nothing" (f.Formalize_faith.score = 0.0);
        let closed = Formalize_faith.validate ~lean:(lean ~refute:(fun _ -> Ok (Formalize_faith.Refuted [])) ()) squares (candidate ": 2 + 2 = 5") ~statement:"2 + 2 = 5" in
        check "a statement without variables is false as it stands" (closed.Formalize_faith.rejected = Some "refuted: the statement is false as it stands");
        (* Finding nothing is said with what was tried; not being able to look is said too. *)
        let survived = Formalize_faith.validate ~lean:(lean ()) squares positive ~statement:printed in
        equal_string "nothing found" "passed no counterexample in 300 sampled cases" (outcome survived "counterexample" ^ " " ^ note survived "counterexample");
        let untested = Formalize_faith.validate ~lean:(lean ~refute:(fun _ -> Error "the statement cannot be tested by sampling") ()) squares positive ~statement:printed in
        equal_string "not tested" "skipped the statement cannot be tested by sampling" (outcome untested "counterexample" ^ " " ^ note untested "counterexample");
        check "which rejects nothing" (untested.Formalize_faith.rejected = None));
    case "a number the statement leaves out, a variable nothing mentions and a trap are flagged without a model" (fun () ->
        let problem = intake "Show that every natural number n greater than 12 satisfies n - 7 > 5, as in Exercise 4." in
        equal_strings "the numbers of the problem" [ "12"; "7"; "5" ] (Formalize_intake.numbers problem);
        equal_strings "a number is found as a whole number only" [ "12" ] (Formalize_faith.missing_numbers problem "(n : Nat) (h : n > 127) : n - 7 > 5");
        equal_strings "none is missing" [] (Formalize_faith.missing_numbers problem "(n : Nat) (h : n > 12) : n - 7 > 5");
        let inspect _ =
          Ok { Formalize_faith.unused = [ "m" ]; traps = [ ("nat-subtraction", "n - 7", "subtraction of natural numbers stops at zero") ]; annotated = "" }
        in
        let f = Formalize_faith.validate ~lean:(lean ~inspect ()) problem (candidate "(n m : Nat) (h : n > 10) : n - 7 > 5") ~statement:"∀ (n m : Nat), n > 10 → n - 7 > 5" in
        equal_string "the number" "flagged the problem mentions 12, and the statement does not" (outcome f "numbers" ^ " " ^ note f "numbers");
        equal_string "the variable" "flagged nothing in the statement mentions m" (outcome f "variables" ^ " " ^ note f "variables");
        equal_string "the trap" "flagged n - 7: subtraction of natural numbers stops at zero" (outcome f "traps" ^ " " ^ note f "traps");
        equal_strings "what flagged it" [ "numbers"; "variables"; "traps" ] (Formalize_faith.flagged f);
        check "the score, without a model" (abs_float (f.Formalize_faith.score -. (0.6 -. 0.2 -. 0.2 -. 0.05)) < 1e-9);
        let j = Formalize_faith.to_json f in
        equal_int "every check is in the record" 8 (List.length (Json.list_or_empty "checks" j));
        check "and the score" (match Json.member "score" j with Some (Json.Float s) -> abs_float (s -. 0.15) < 1e-9 | _ -> false);
        check "no agreement where nobody was asked" (Json.member "agreement" j = None)) ]

(* ---- the run ---- *)

let run_tests =
  [ case "the best statement is chosen, and what became of every candidate is said" (fun () ->
        let compile = objecting [ ("ℤ", "Unknown identifier `ℤ`"); ("Real", "Unknown identifier `Real`"); ("ℝ", "Unknown identifier `ℝ`") ] in
        let repair = answering [ Ok "(n : Int) (h : n < 0) : n * n > 0"; Ok "(x : ℝ) : x ^ 2 > 0"; Ok "(x : Real) (h : x < 0) : x ^ 2 > 0" ] (ref []) in
        let vacuous c = Ok (if Text.contains (signature c) "(n : Nat)" then Some "the hypothesis h cannot hold (shown by omega)" else None) in
        let refute c = Ok (if Text.contains (signature c) "≤ 0" then Formalize_faith.Refuted [ ("n", "0") ] else Formalize_faith.Survived "300 sampled cases") in
        let judge ~problem:_ ~english = Ok (if Text.contains english "n * n" then (8, [ "The square is written as a product." ]) else (10, [])) in
        let run =
          is_ok "run"
            (Formalize_run.run
               ~settings:{ Formalize_run.default_settings with Formalize_run.candidates = 5 }
               (tools ~compile ~repair ~lean:(lean ~vacuous ~refute ()) ~model:(reading ~judge ())
                  (Ok
                     [ "(n : ℤ) (h : n < 0) : n * n > 0"; "(n : Nat) (h : n < 0) : 0 < n ^ 2"; "(n : Int) (h : n ≤ 0) : 0 < n ^ 2"; "(n : Int) (h : n < 0) : 0 < n ^ 2";
                       "(x : Real) (h : x < 0) : 0 < x ^ 2" ]))
               squares)
        in
        equal_strings "what became of each" [ "kept"; "rejected"; "rejected"; "chosen"; "failed" ] (verdicts run);
        (match Formalize_run.chosen run with
         | Some entry ->
           equal_int "the statement with the best score" 4 entry.Formalize_run.drafted.Formalize_candidate.id;
           check "is first" (entry.Formalize_run.rank = Some 1)
         | None -> fail "a statement was expected");
        check "the one behind it is second" ((List.hd run.Formalize_run.entries).Formalize_run.rank = Some 2);
        equal_string "the repaired statement is the one that is kept" "(n : Int) (h : n < 0) : n * n > 0" (signature (Formalize_run.final (List.hd run.Formalize_run.entries)));
        check "what is out has no rank" (List.for_all (fun (e : Formalize_run.entry) -> e.Formalize_run.rank = None) (List.filteri (fun i _ -> List.mem i [ 1; 2; 4 ]) run.Formalize_run.entries));
        check "formalized" (run.Formalize_run.status = Formalize_run.Formalized);
        equal_strings "the counts"
          [ "drafted 5"; "elaborated_first_try 3"; "repaired 1"; "elaborated 4"; "failed 1"; "duplicates 0"; "rejected 2"; "kept 2" ]
          (List.map (fun (name, n) -> Printf.sprintf "%s %d" name n) (Formalize_run.counts run));
        equal_string "in a line" "formalized: candidate 4 of 5 (1 repaired, 1 did not elaborate, 2 rejected)" (Formalize_run.summary run);
        check "the exit status is success" (Formalize_run.exit_code run = Exit_code.Success);
        let text = Formalize_run.render run in
        contains "the problem" text "problem    Show that the square of a negative integer is positive.";
        contains "a repair, with what Lean said" text "drafted    does not elaborate  unknown-constant";
        contains "the repair that worked" text "elaborates after 1 repair\n";
        contains "a rejection" text "→ rejected  refuted: it fails at n = 0";
        contains "a failure" text "→ failed  it does not elaborate after 2 repair round(s)";
        contains "the choice" text "→ chosen  score 1.00";
        check "no colour unless asked" (not (Text.contains text "\027["));
        check "colour when asked" (Text.contains (Formalize_run.render ~color:true run) "\027[32m"));
    case "of two statements with the same score, the one the model put first is chosen" (fun () ->
        let run = is_ok "run" (Formalize_run.run (tools ~model:(reading ()) (Ok [ "(n : Nat) : n ^ 2 ≥ n"; "(n : Int) : n ^ 2 ≥ n" ])) (intake "Prove that n² ≥ n for every number n.")) in
        equal_strings "what became of each" [ "chosen"; "kept" ] (verdicts run));
    case "a chosen statement that a check flagged is said to be flagged, and no statement is said too" (fun () ->
        let inspect _ = Ok { sound with Formalize_faith.traps = [ ("nat-subtraction", "a - b", "subtraction of natural numbers stops at zero") ] } in
        let judge ~problem:_ ~english:_ = Ok (7, [ "The statement assumes b ≤ a." ]) in
        let flagged =
          is_ok "run"
            (Formalize_run.run (tools ~lean:(lean ~inspect ()) ~model:(reading ~judge ()) (Ok [ "(a b : Nat) (h : b ≤ a) : a - b + b = a" ])) (intake "Show that (a - b) + b = a."))
        in
        check "flagged" (flagged.Formalize_run.status = Formalize_run.Flagged);
        equal_string "by what" "flagged by traps and round-trip" flagged.Formalize_run.note;
        equal_string "in a line" "chosen with a flag: candidate 1 of 1, flagged by traps and round-trip" (Formalize_run.summary flagged);
        check "the exit status says it is not settled" (Formalize_run.exit_code flagged = Exit_code.Not_verified);
        check "a statement is chosen all the same" (Formalize_run.chosen flagged <> None);
        let status tools = (is_ok "run" (Formalize_run.run tools squares)).Formalize_run.status in
        let none = is_ok "run" (Formalize_run.run (tools ~compile:(objecting [ ("Even", unknown_even) ]) (Ok [ "(a : Nat) : Even a" ])) squares) in
        check "nothing elaborates" (none.Formalize_run.status = Formalize_run.Nothing);
        equal_string "and why" "no candidate elaborates" none.Formalize_run.note;
        equal_string "in a line" "no statement: no candidate elaborates (1 did not elaborate)" (Formalize_run.summary none);
        check "no file to write" (Formalize_run.lean_text none = None);
        check "the exit status says so" (Formalize_run.exit_code none = Exit_code.Not_verified);
        check "everything rejected" (status (tools ~lean:(lean ~vacuous:(fun _ -> Ok (Some "no")) ()) (Ok [ "(a : Nat) : a = a" ])) = Formalize_run.Nothing);
        equal_string "nothing drafted" "the model returned no statement" (is_ok "run" (Formalize_run.run (tools (Ok [])) squares)).Formalize_run.note;
        equal_string "no answer at all is an error" "the provider is down" (is_error "run" (Formalize_run.run (tools (Error "the provider is down")) squares)));
    case "the chosen statement is written as a Lean file with what the checks found" (fun () ->
        let drafts ~count:_ _ =
          Ok [ { (candidate ~reading:"An integer below zero." ~imports:[ "Std" ] "(n : Int) (h : n < 0) : 0 < n ^ 2") with Formalize_candidate.provenance = by_model } ]
        in
        let model = reading ~back:(fun ~statement:_ -> Ok "For every integer n below zero, the square of n is above zero.") ~judge:(fun ~problem:_ ~english:_ -> Ok (9, [])) () in
        let run =
          is_ok "run"
            (Formalize_run.run
               ~settings:{ Formalize_run.default_settings with Formalize_run.name = "square_pos"; imports = [ "Std" ] }
               { (tools ~model (Ok [])) with Formalize_run.draft = drafts }
               squares)
        in
        equal_string "the file"
          (String.concat "\n"
             [ "import Std"; ""; "/-"; "The problem, as it was posed:"; ""; "  Show that the square of a negative integer is positive."; "";
               "The statement below was chosen from 1 candidate. Lean elaborates it; nothing here proves it."; "Its reading of the problem: An integer below zero."; "";
               "Read back into English, the statement says:"; ""; "  For every integer n below zero, the square of n is above zero."; ""; "What the checks found:"; "";
               "  vacuity           passed"; "  counterexample    passed   no counterexample in 300 sampled cases"; "  triviality        passed"; "  numbers           passed";
               "  variables         passed"; "  traps             passed"; "  back-translation  passed"; "  round-trip        passed   agreement 9 of 10"; "";
               "That a statement says what was meant is a matter of evidence. Read it before you rely on it."; "-/"; "set_option autoImplicit false in";
               "theorem square_pos (n : Int) (h : n < 0) : 0 < n ^ 2 := by"; "  sorry"; "" ])
          (Option.get (Formalize_run.lean_text run)));
    case "a run is a document of its schema" (fun () ->
        let compile = objecting [ ("ℤ", "Unknown identifier `ℤ`") ] in
        let repair = answering [ Ok "(n : Int) (h : n < 0) : n ^ 2 > 0" ] (ref []) in
        let same (a : Formalize_candidate.t) (b : Formalize_candidate.t) = if a.Formalize_candidate.id = 1 && Text.contains (signature b) "n ^ 2 > 0" then Some "definitional" else None in
        let refute c = Ok (if Text.contains (signature c) "≤ 0" then Formalize_faith.Refuted [ ("n", "0") ] else Formalize_faith.Survived "300 sampled cases") in
        let run =
          is_ok "run"
            (Formalize_run.run
               (tools ~compile ~repair ~same ~lean:(lean ~refute ()) ~model:(reading ())
                  (Ok [ "(n : Int) (h : n < 0) : 0 < n ^ 2"; "(n : Int) (h : n ≤ 0) : 0 < n ^ 2"; "(n : ℤ) (h : n < 0) : n ^ 2 > 0" ]))
               squares)
        in
        let j = Formalize_run.to_json run in
        equal_string "the schema" "maithic.formalize.run/1" (Json.string_or "" "schema" j);
        equal_string "its name" Formalize_run.schema (Json.string_or "" "schema" j);
        let schema = Json_schema.of_json (Support.json "schemas/formalize/run.schema.json") in
        equal_strings "it is a document of its schema" [] (List.map Json_schema.error_to_string (Json_schema.validate schema j));
        equal_string "the status" "formalized" (Json.string_or "" "status" j);
        equal_int "the chosen candidate" 1 (Json.int_or 0 "chosen" j);
        (match Json.list_or_empty "candidates" j with
         | [ first; second; third ] ->
           equal_string "the first" "chosen" (Json.string_or "" "verdict" first);
           equal_int "its rank" 1 (Json.int_or 0 "rank" first);
           equal_int "its agreement" 10 (Json.int_or 0 "agreement" first);
           equal_string "the second" "rejected" (Json.string_or "" "verdict" second);
           equal_string "and why" "refuted: it fails at n = 0" (Json.string_or "" "reason" second);
           equal_string "the third" "duplicate" (Json.string_or "" "verdict" third);
           equal_string "as it was drafted" "(n : ℤ) (h : n < 0) : n ^ 2 > 0" (Json.string_or "" "drafted" third);
           equal_string "and as it stood at the end" "(n : Int) (h : n < 0) : n ^ 2 > 0" (Json.string_or "" "statement" third);
           (match Json.member "duplicate_of" third with
            | Some d ->
              equal_int "the statement it repeats" 1 (Json.int_or 0 "id" d);
              equal_string "how" "definitional" (Json.string_or "" "relation" d)
            | None -> fail "no duplicate_of");
           equal_string "in words" "it says what candidate 1 says (the same once definitions are unfolded)" (Json.string_or "" "reason" third)
         | _ -> fail "three candidates were expected");
        (match Json.member "summary" j with
         | Some s ->
           equal_int "drafted" 3 (Json.int_or 0 "drafted" s);
           equal_int "repaired" 1 (Json.int_or 0 "repaired" s);
           equal_int "duplicates" 1 (Json.int_or 0 "duplicates" s)
         | None -> fail "no summary");
        contains "the Lean file is in the record" (Json.string_or "" "lean" j) "theorem problem (n : Int) (h : n < 0) : 0 < n ^ 2 := by\n  sorry\n";
        (* A run without a statement is a document too. *)
        let none = Formalize_run.to_json (is_ok "run" (Formalize_run.run (tools ~compile:(objecting [ ("Even", unknown_even) ]) (Ok [ "(a : Nat) : Even a" ])) squares)) in
        equal_strings "without a statement" [] (List.map Json_schema.error_to_string (Json_schema.validate schema none));
        check "nothing is chosen" (Json.member "chosen" none = None && Json.member "lean" none = None));
    case "the recorded runs say what the documentation says about them" (fun () ->
        let schema = Json_schema.of_json (Support.json "schemas/formalize/run.schema.json") in
        let recorded name = Support.json ("fixtures/formalize/" ^ name ^ ".json") in
        let verdicts j = List.map (fun c -> Json.string_or "" "verdict" c) (Json.list_or_empty "candidates" j) in
        let squares = recorded "negative-square" in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate schema squares));
        equal_string "four statements, one left" "formalized" (Json.string_or "" "status" squares);
        equal_strings "what became of each" [ "chosen"; "rejected"; "rejected"; "duplicate" ] (verdicts squares);
        equal_strings "why the two were rejected"
          [ "vacuous: the hypothesis h cannot hold (shown by contradiction)"; "refuted: it fails at n = 0" ]
          (List.filter_map (fun c -> if Json.string_or "" "verdict" c = "rejected" then Some (Json.string_or "" "reason" c) else None) (Json.list_or_empty "candidates" squares));
        let subtraction = recorded "subtraction" in
        equal_string "a problem that is false as posed" "flagged" (Json.string_or "" "status" subtraction);
        equal_strings "what became of each" [ "rejected"; "chosen"; "kept" ] (verdicts subtraction);
        equal_string "by what" "flagged by traps and round-trip" (Json.string_or "" "note" subtraction);
        let bound = recorded "square-bound" in
        equal_strings "repairs, a duplicate and a failure" [ "chosen"; "duplicate"; "kept"; "failed" ] (verdicts bound);
        (match Json.member "summary" bound with
         | Some s ->
           equal_int "two elaborate as drafted" 2 (Json.int_or 0 "elaborated_first_try" s);
           equal_int "one after repair" 1 (Json.int_or 0 "repaired" s);
           equal_int "one not at all" 1 (Json.int_or 0 "failed" s)
         | None -> fail "no summary");
        let alone = recorded "subtraction-no-ai" in
        equal_strings "with Lean alone" [ "rejected"; "chosen" ] (verdicts alone);
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate schema alone));
        contains "the file that was written" (Support.read "fixtures/formalize/lean/NegativeSquare.lean") "theorem problem (n : Int) (h : n < 0) : 0 < n ^ 2 := by\n  sorry\n") ]

let () =
  run "formalize"
    [ ("intake", ingest_tests); ("drafting", draft_tests); ("elaboration", elab_tests); ("faithfulness", faith_tests); ("run", run_tests) ]
