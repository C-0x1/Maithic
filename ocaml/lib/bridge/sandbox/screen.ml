(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type finding = { construct : string; reason : string }

let max_candidate_chars = 20000

(* Kept in step with lean/MaithicBridge/Server/Guard.lean. *)
let denied_words =
  [ "axiom"; "unsafe"; "partial"; "extern"; "implemented_by"; "initialize"; "builtin_initialize"; "run_cmd"; "run_elab";
    "run_meta"; "elab"; "elab_rules"; "macro"; "macro_rules"; "syntax"; "notation"; "infix"; "infixl"; "infixr"; "prefix";
    "postfix"; "attribute"; "instance"; "set_option"; "import"; "include"; "open"; "export"; "namespace"; "section"; "end";
    "universe"; "variable"; "mutual"; "inductive"; "structure"; "class"; "abbrev"; "opaque"; "def"; "theorem"; "lemma";
    "example"; "deriving"; "native_decide"; "sorry"; "admit"; "stop"; "run_tac"; "by_elab"; "eval_expr"; "csimp";
    "reduce_bool"; "local"; "scoped"; "private"; "protected"; "noncomputable"; "register_option"; "register_simp_attr";
    "declare_syntax_cat"; "#eval"; "#eval!"; "#exit"; "#print"; "#check"; "#reduce"; "#synth"; "#guard"; "#guard_msgs";
    "#help"; "#html"; "#lint"; "#find"; "#widget"; "#time" ]

let denied_prefixes =
  [ "IO."; "System."; "Lean.Elab."; "Lean.Meta."; "Lean.Compiler."; "Lean.IR."; "Lean.Environment." ]

let denied_names =
  [ "Lean.ofReduceBool"; "Lean.ofReduceNat"; "Lean.reduceBool"; "Lean.reduceNat"; "Lean.trustCompiler"; "IO"; "unsafeCast";
    "unsafeIO"; "unsafeBaseIO"; "lcProof"; "lcUnreachable"; "sorryAx"; "ptrAddrUnsafe" ]

let is_word_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '.' | '#' | '!' | '?' -> true
  | _ -> false (* as in the Lean guard, characters outside ASCII end a word *)

let words text =
  let acc = ref [] and current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then begin
      acc := Buffer.contents current :: !acc;
      Buffer.clear current
    end
  in
  String.iter (fun c -> if is_word_char c then Buffer.add_char current c else flush ()) text;
  flush ();
  List.rev !acc

let rec strip_root word =
  if Text.starts_with ~prefix:"_root_." word then strip_root (String.sub word 7 (String.length word - 7)) else word

let word_finding raw =
  let word = strip_root raw in
  if List.mem word denied_words then Some { construct = word; reason = Printf.sprintf "forbidden keyword '%s'" word }
  else if List.mem word denied_names then Some { construct = word; reason = Printf.sprintf "forbidden name '%s'" word }
  else
    match List.find_opt (fun prefix -> Text.starts_with ~prefix word) denied_prefixes with
    | Some prefix -> Some { construct = word; reason = Printf.sprintf "forbidden name '%s' (matches '%s')" word prefix }
    | None -> None

let violations candidate =
  let size =
    if Text.utf8_length candidate > max_candidate_chars then
      [ { construct = "length"; reason = Printf.sprintf "candidate longer than %d characters" max_candidate_chars } ]
    else []
  in
  let quoted =
    if Text.contains candidate "\xC2\xAB" || Text.contains candidate "\xC2\xBB" then
      [ { construct = "quoted identifier"; reason = "forbidden character (quoted identifier)" } ]
    else []
  in
  size @ quoted @ List.filter_map word_finding (words candidate)

let admits candidate = violations candidate = []

let secret_markers = [ "KEY"; "TOKEN"; "SECRET"; "PASSWORD"; "PASSWD"; "CREDENTIAL"; "AUTH" ]

let is_secret_name name =
  let upper = String.uppercase_ascii name in
  List.exists (fun marker -> Text.contains upper marker) secret_markers

let allowed_exact = [ "PATH"; "HOME"; "LANG"; "LANGUAGE"; "TMPDIR"; "TEMP"; "TMP"; "TERM"; "USER"; "LOGNAME"; "SHELL"; "TZ" ]
let allowed_prefixes = [ "LC_"; "LEAN_"; "LAKE_"; "ELAN_"; "MAITHIC_FIXTURES" ]

let scrub_env environment =
  Array.to_list environment
  |> List.filter_map (fun binding ->
         match String.index_opt binding '=' with
         | None -> None
         | Some i ->
           let name = String.sub binding 0 i in
           let value = String.sub binding (i + 1) (String.length binding - i - 1) in
           let allowed =
             List.mem name allowed_exact || List.exists (fun prefix -> Text.starts_with ~prefix name) allowed_prefixes
           in
           if allowed && not (is_secret_name name) then Some (name, value) else None)
