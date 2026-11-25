(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The README image check.

    The README explains the project largely through figures, so its images
    are checked like code: every image must be a file of the repository,
    must carry alternative text, and the README must show at least the
    number of images that [PROJECT.toml] asks for. *)

open Maithic_base

type image = { src : string; alt : string; line : int }

(* The value of an HTML attribute in a tag text. *)
let attribute tag name =
  let pattern = name ^ "=\"" in
  match Text.find_sub tag pattern with
  | None -> None
  | Some i ->
    let start = i + String.length pattern in
    Option.map (fun stop -> String.sub tag start (stop - start)) (String.index_from_opt tag start '"')

(** Every image of a Markdown text: HTML [<img>] tags and [![alt](src)]. *)
let images text =
  let found = ref [] in
  List.iteri
    (fun index line ->
      let number = index + 1 in
      let rec html from =
        match Text.find_sub ~from line "<img" with
        | None -> ()
        | Some i ->
          let stop = match String.index_from_opt line i '>' with Some s -> s | None -> String.length line - 1 in
          let tag = String.sub line i (stop - i + 1) in
          (match attribute tag "src" with
           | Some src -> found := { src; alt = Option.value (attribute tag "alt") ~default:""; line = number } :: !found
           | None -> ());
          html (stop + 1)
      in
      html 0;
      let rec markdown from =
        match Text.find_sub ~from line "![" with
        | None -> ()
        | Some i ->
          (match Text.find_sub ~from:i line "](" with
           | Some middle ->
             (match String.index_from_opt line middle ')' with
              | Some stop ->
                let target = String.sub line (middle + 2) (stop - middle - 2) in
                let src = match String.index_opt target ' ' with Some sp -> String.sub target 0 sp | None -> target in
                found := { src; alt = String.sub line (i + 2) (middle - i - 2); line = number } :: !found;
                markdown (stop + 1)
              | None -> ())
           | None -> ())
      in
      markdown 0)
    (String.split_on_char '\n' text);
  List.rev !found

let is_remote src = Text.starts_with ~prefix:"http://" src || Text.starts_with ~prefix:"https://" src || Text.starts_with ~prefix:"data:" src

type report = { images : image list; local : int; problems : string list }

(** Checks the README of a repository. *)
let check ?(file = "README.md") ~min_images root =
  let text = Repo.read root file in
  let all = images text in
  let local = List.filter (fun i -> not (is_remote i.src)) all in
  let problems =
    (if text = "" then [ file ^ " is missing or empty" ] else [])
    @ List.concat_map
        (fun i ->
          let path = match String.index_opt i.src '#' with Some h -> String.sub i.src 0 h | None -> i.src in
          (if is_remote i.src then [ Printf.sprintf "%s:%d: image %s is not a file of the repository" file i.line i.src ]
           else if not (Fs.exists Fs.(root / path)) then [ Printf.sprintf "%s:%d: image file %s does not exist" file i.line path ]
           else [])
          @ if String.trim i.alt = "" then [ Printf.sprintf "%s:%d: image %s has no alternative text" file i.line i.src ] else [])
        all
    @
    let distinct = List.length (List.sort_uniq compare (List.map (fun i -> i.src) local)) in
    if distinct < min_images then [ Printf.sprintf "%s shows %d distinct images; PROJECT.toml asks for at least %d" file distinct min_images ]
    else []
  in
  { images = all; local = List.length local; problems }

let min_images_of_project doc = Option.value (Option.bind (Json.path [ "readme"; "min_images" ] doc) Json.int_opt) ~default:0
