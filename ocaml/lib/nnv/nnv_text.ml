(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** From what a verifier returns to a Lean file that states the property as a
    theorem and proves it with the verified checker. *)

open Maithic_base

let ( let* ) = Result.bind

(** An exact number as the documents write it: an integer or a fraction, with
    an optional sign. Nothing else may reach a Lean file. *)
let is_exact text =
  let digits s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s in
  let unsigned = if Text.starts_with ~prefix:"-" text then String.sub text 1 (String.length text - 1) else text in
  match String.split_on_char '/' unsigned with
  | [ n ] -> digits n
  | [ n; d ] -> digits n && digits d && String.exists (fun c -> c <> '0') d
  | _ -> false

(** A number in Lean's notation; a negative one in brackets. *)
let lean_number text =
  if not (is_exact text) then Error (Printf.sprintf "'%s' is not an exact number: an integer or a fraction is expected" text)
  else Ok (if Text.starts_with ~prefix:"-" text then "(" ^ text ^ ")" else text)

let all f items = List.fold_right (fun item acc -> let* acc = acc in let* x = f item in Ok (x :: acc)) items (Ok [])
let number_of = function Json.String text -> lean_number text | _ -> Error "a number is not written as text"
let list_of what = function Json.List items -> Ok items | _ -> Error (what ^ " is not a list")

let interval = function
  | Json.List [ lo; hi ] ->
    let* lo = number_of lo in
    let* hi = number_of hi in
    Ok (Printf.sprintf "⟨%s, %s⟩" lo hi)
  | _ -> Error "an interval is not a pair of bounds"

let box json =
  let* items = list_of "a box" json in
  let* intervals = all interval items in
  Ok ("[" ^ String.concat ", " intervals ^ "]")

let neuron json =
  let* weights = Result.bind (list_of "the weights of a neuron" (Option.value ~default:Json.Null (Json.member "weights" json))) (all number_of) in
  let* bias = number_of (Option.value ~default:Json.Null (Json.member "bias" json)) in
  Ok (Printf.sprintf "⟨[%s], %s⟩" (String.concat ", " weights) bias)

let layer json =
  let* neurons = Result.bind (list_of "the neurons of a layer" (Option.value ~default:Json.Null (Json.member "neurons" json))) (all neuron) in
  let* activation =
    match Json.string_or "" "activation" json with
    | "relu" -> Ok ".relu"
    | "identity" -> Ok ".identity"
    | other -> Error (Printf.sprintf "'%s' is not an activation" other)
  in
  Ok (Printf.sprintf "  { neurons := [%s], activation := %s }" (String.concat ", " neurons) activation)

let namespace = "Maithic.NNV.Job"
let holds = "property_holds"
let fails = "property_fails"

(** What a verdict claims, and the Lean file that puts the claim to the kernel. *)
type lean = {
  theorem : string;  (** The full name of the theorem that states the claim. *)
  statement : string;  (** The claim in words a report can show. *)
  source : string;
}

(** The Lean file for a verdict of the schema [maithic.nnv.verdict/1]: the
    network, the boxes, and one theorem. For a verified property the theorem
    says that the property holds and is proved by running the certificate
    checker in the kernel; for a violated one it says that the property
    fails, by running the counterexample checker. [None] for a verdict that
    claims nothing. *)
let of_verdict verdict =
  let member name = Option.value ~default:Json.Null (Json.member name verdict) in
  let* layers = Result.bind (list_of "the network" (member "network")) (all layer) in
  let* input = box (member "input") in
  let* claimed = box (member "output") in
  let head =
    [ "-- Written by `maithic nnv verify`. The kernel runs a checker that is proved sound; nothing else is trusted.";
      "import Maithic.NN.Cert.Property"; ""; "open Maithic.NN"; ""; "namespace " ^ namespace; "";
      "def net : Network := [\n" ^ String.concat ",\n" layers ^ "]"; ""; "def input : Box := " ^ input; ""; "def claimed : Box := " ^ claimed; "" ]
  in
  match Json.string_or "" "verdict" verdict with
  | "verified" ->
    let* boxes = Result.bind (list_of "the certificate" (member "boxes")) (all box) in
    let body =
      [ "def certificate : Certificate := [\n  " ^ String.concat ",\n  " boxes ^ "]"; "";
        "/-- Every point of the input box is mapped into the claimed box. -/";
        Printf.sprintf "theorem %s : Holds net input claimed :=" holds; "  checkProperty_sound (cert := certificate) (by decide +kernel)"; ""; "end " ^ namespace ]
    in
    Ok (Some { theorem = namespace ^ "." ^ holds; statement = "every point of the input box is mapped into the claimed box"; source = String.concat "\n" (head @ body) ^ "\n" })
  | "violated" ->
    let* point =
      match Json.member "counterexample" verdict with
      | Some c -> Result.bind (list_of "the counterexample" (Option.value ~default:Json.Null (Json.member "point" c))) (all number_of)
      | None -> Error "a violated property comes without a counterexample"
    in
    let body =
      [ "def point : List Rat := [" ^ String.concat ", " point ^ "]"; "";
        "/-- Some point of the input box is mapped outside the claimed box. -/";
        Printf.sprintf "theorem %s : ¬ Holds net input claimed :=" fails; "  not_holds_of_counterexample (xs := point) (by decide +kernel)"; ""; "end " ^ namespace ]
    in
    Ok (Some { theorem = namespace ^ "." ^ fails; statement = "some point of the input box is mapped outside the claimed box"; source = String.concat "\n" (head @ body) ^ "\n" })
  | "unknown" -> Ok None
  | other -> Error (Printf.sprintf "'%s' is not a verdict; the verdicts are verified, violated and unknown" other)
