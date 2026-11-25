(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Helpers shared by the test executables. *)

open Maithic_base

(** The repository root: the nearest directory at or above the current one
    that holds [PROJECT.toml]. Tests read fixtures relative to it. *)
let root =
  lazy
    (let rec up dir depth =
       if Fs.exists Fs.(dir / "PROJECT.toml") then dir
       else
         let parent = Filename.dirname dir in
         if parent = dir || depth > 64 then failwith "PROJECT.toml not found above the test directory" else up parent (depth + 1)
     in
     up (Sys.getcwd ()) 0)

let path rel = Fs.(Lazy.force root / rel)
let read rel = match Fs.read (path rel) with Ok text -> text | Error e -> failwith e
let json rel = Json.parse_exn (read rel)

(** A JSON file anywhere, by its own path. *)
let json_file file = match Fs.read file with Ok text -> Json.parse_exn text | Error e -> failwith e

(** The stub Bridge built next to the tests. *)
let stub_exe =
  lazy
    (let candidates = [ "stub/bridge_stub.exe"; "test/stub/bridge_stub.exe"; path "ocaml/_build/default/test/stub/bridge_stub.exe" ] in
     match List.find_opt Sys.file_exists candidates with
     | Some exe -> if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe
     | None -> failwith "bridge_stub.exe not found")

let stub ?(options = []) transcript = (Lazy.force stub_exe :: path ("fixtures/bridge/transcripts/" ^ transcript ^ ".jsonl") :: options)

(** A key-shaped string assembled at run time, so that no source file
    contains one. *)
let fake_key provider_prefix = provider_prefix ^ "-" ^ String.concat "" (List.init 8 (fun i -> Printf.sprintf "t%dE5" i))

(** An environment lookup backed by a list. *)
let env bindings name = List.assoc_opt name bindings
