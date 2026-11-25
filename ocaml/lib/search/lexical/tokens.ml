(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Words of names, of statements and of prose, in one vocabulary: the name
    [Nat.add_comm], the statement [a + b = b + a] and the sentence "addition
    is commutative" should have words in common. *)

(* What Lean's notation is called in the names of its lemmas. *)
let symbols =
  [ ("↔", "iff"); ("→", "imp"); ("∧", "and"); ("∨", "or"); ("¬", "not"); ("≤", "le"); ("≥", "ge"); ("≠", "ne"); ("∈", "mem"); ("∉", "mem"); ("∣", "dvd");
    ("⊆", "subset"); ("∪", "union"); ("∩", "inter"); ("∀", "forall"); ("∃", "exists"); ("∘", "comp"); ("×", "prod"); ("++", "append"); ("::", "cons");
    ("+", "add"); ("-", "sub"); ("*", "mul"); ("/", "div"); ("%", "mod"); ("^", "pow"); ("<", "lt"); (">", "gt"); ("=", "eq") ]

(* Words of prose that say what a symbol says. *)
let synonyms =
  [ ("addition", "add"); ("adding", "add"); ("added", "add"); ("plus", "add"); ("sum", "add"); ("multiplying", "mul"); ("multiplied", "mul"); ("multiplication", "mul"); ("times", "mul"); ("product", "mul"); ("subtraction", "sub"); ("minus", "sub");
    ("division", "div"); ("divides", "dvd"); ("power", "pow"); ("equal", "eq"); ("equals", "eq"); ("equality", "eq"); ("commutative", "comm");
    ("commutativity", "comm"); ("commutes", "comm"); ("associative", "assoc"); ("associativity", "assoc"); ("member", "mem"); ("membership", "mem");
    ("element", "mem"); ("elements", "mem"); ("permutation", "perm"); ("length", "length"); ("sorted", "sorted"); ("sorting", "sort"); ("sorts", "sort");
    ("concatenate", "append"); ("concatenation", "append"); ("smaller", "lt"); ("less", "lt"); ("greater", "gt"); ("implies", "imp"); ("zero", "0"); ("one", "1") ]

let stop_words = [ "the"; "a"; "an"; "of"; "is"; "are"; "in"; "to"; "and"; "or"; "it"; "its"; "that"; "this"; "for"; "with"; "by"; "on"; "as"; "be"; "at"; "when"; "every"; "each"; "any"; "there"; "if"; "then" ]

(* Parts of an identifier: at dots and underscores, and where a small letter meets a capital one. *)
let split_identifier word =
  let parts = ref [] and current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then begin
      parts := Buffer.contents current :: !parts;
      Buffer.clear current
    end
  in
  String.iteri
    (fun i c ->
      match c with
      | '.' | '_' | '\'' | '!' | '?' -> flush ()
      | 'A' .. 'Z' when i > 0 && (match word.[i - 1] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false) ->
        flush ();
        Buffer.add_char current (Char.lowercase_ascii c)
      | c -> Buffer.add_char current (Char.lowercase_ascii c))
    word;
  flush ();
  List.rev !parts

let is_word_char = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '\'' -> true | _ -> false

(** The words of a text. Identifiers are taken apart, notation is replaced by
    its name, and single letters, which are variables, are dropped. With
    [~prose:true] common words are dropped too, and words that name an
    operation are replaced by the name of the operation. *)
let of_text ?(prose = false) text =
  let out = ref [] in
  (* a single letter is a variable, and tells nothing about what a statement is about *)
  let variable word = String.length word = 1 && match word.[0] with 'a' .. 'z' -> true | _ -> false in
  let emit word =
    let word = if prose then Option.value ~default:word (List.assoc_opt word synonyms) else word in
    if word <> "" && (not (variable word)) && not (prose && List.mem word stop_words) then out := word :: !out
  in
  let n = String.length text in
  let rec go i =
    if i >= n then ()
    else if is_word_char text.[i] then begin
      let j = ref i in
      while !j < n && is_word_char text.[!j] do incr j done;
      List.iter emit (split_identifier (String.sub text i (!j - i)));
      go !j
    end
    else
      match List.find_opt (fun (symbol, _) -> let l = String.length symbol in i + l <= n && String.sub text i l = symbol) symbols with
      | Some (symbol, name) ->
        emit name;
        go (i + String.length symbol)
      | None -> go (i + 1)
  in
  go 0;
  List.rev !out

(** The words of a name, and the name itself in lower case, so that a query
    for the whole name finds it. *)
let of_name name = String.lowercase_ascii name :: split_identifier name
