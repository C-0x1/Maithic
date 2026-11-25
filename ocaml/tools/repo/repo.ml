(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The repository as the development tools see it: where its root is, which
    files belong to it, and what [PROJECT.toml] says. *)

open Maithic_base

(** The nearest directory at or above [dir] that contains [PROJECT.toml]. *)
let find_root dir =
  let rec up dir depth =
    if Fs.exists Fs.(dir / "PROJECT.toml") then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir || depth > 64 then None else up parent (depth + 1)
  in
  up dir 0

(** Directories that hold build products or tool state, never sources. *)
let skipped_dirs = [ ".git"; "_build"; ".lake"; "node_modules"; ".Rproj.user"; "renv"; ".julia"; "_opam"; ".maithic"; "dist"; ".library" ]

let skipped_suffixes = [ ".Rcheck"; ".tar.gz"; ".o"; ".so"; ".olean"; ".ilean"; ".cmi"; ".cmx"; ".cmo"; ".DS_Store"; ".swp"; ".orig"; ".rej" ]

let skip rel =
  let base = Filename.basename rel in
  List.mem base skipped_dirs || List.exists (fun suffix -> Text.ends_with ~suffix base) skipped_suffixes

(** Every file of the repository, as sorted paths relative to the root.
    Build directories and tool state are left out, which makes the list the
    same in a checkout and in an unpacked release archive. *)
let files root = Fs.walk ~skip root

let project root = Toml.parse_file Fs.(root / "PROJECT.toml")

(** [true] when the file is text that the scans should read. *)
let is_text rel =
  List.mem (Fs.extension rel)
    [ ".ml"; ".mli"; ".mll"; ".mly"; ".lean"; ".jl"; ".r"; ".rmd"; ".qmd"; ".md"; ".toml"; ".json"; ".jsonl"; ".yml"; ".yaml"; ".txt";
      ".sh"; ".py"; ".ts"; ".js"; ".css"; ".html"; ".svg"; ".cff"; ".opam"; ".sql"; ".tex"; ".bib"; ".csv"; ".rd"; ".el"; ".vim"; "" ]
  || List.mem (Filename.basename rel) [ "Makefile"; "dune"; "dune-project"; "DESCRIPTION"; "NAMESPACE"; "LICENSE"; "NOTICE" ]

(** Reads a file of the repository; unreadable files read as empty. *)
let read root rel = match Fs.read Fs.(root / rel) with Ok text -> text | Error _ -> ""
