(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type source = Explicit | Environment of string | Credentials_file of string

let source_to_string = function
  | Explicit -> "command line"
  | Environment name -> "environment variable " ^ name
  | Credentials_file path -> "credentials file " ^ path

let credentials_path ~home = Fs.(home / "credentials.toml")

let read_credentials ~home =
  match Toml.parse_file (credentials_path ~home) with
  | Ok doc ->
    (match Json.member "keys" doc with
     | Some (Json.Obj members) -> List.filter_map (fun (k, v) -> Option.map (fun s -> (k, s)) (Json.string_opt v)) members
     | _ -> [])
  | Error _ -> []

let lookup ?explicit ~getenv ~home (provider : Model_registry.provider) =
  if provider.Model_registry.key_env = "" then Some ("", Explicit)
  else
    match explicit with
    | Some key when key <> "" -> Some (key, Explicit)
    | _ ->
      (match getenv provider.Model_registry.key_env with
       | Some key when String.trim key <> "" -> Some (String.trim key, Environment provider.Model_registry.key_env)
       | _ ->
         (match List.assoc_opt provider.Model_registry.name (read_credentials ~home) with
          | Some key when key <> "" -> Some (key, Credentials_file (credentials_path ~home))
          | _ -> None))

let store ~home ~provider key =
  let path = credentials_path ~home in
  let entries = List.remove_assoc provider (read_credentials ~home) in
  let entries = if key = "" then entries else entries @ [ (provider, key) ] in
  let text =
    "# Maithic credentials. Keep this file private; it is readable by its owner only.\n[keys]\n"
    ^ String.concat "" (List.map (fun (k, v) -> Printf.sprintf "%s = %s\n" k (Toml.escape_string v)) entries)
  in
  match Fs.mkdir_p home with
  | Error _ as e -> e
  | Ok () ->
    (* Create the file with owner-only permissions before any key is written to it. *)
    (match
       let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
       Unix.fchmod fd 0o600;
       let written = Unix.write_substring fd text 0 (String.length text) in
       Unix.close fd;
       written
     with
     | n when n = String.length text -> Ok ()
     | _ -> Error ("short write to " ^ path)
     | exception Unix.Unix_error (err, _, _) -> Error (Printf.sprintf "cannot write %s: %s" path (Unix.error_message err)))

let redact key =
  let n = String.length key in
  if n = 0 then "(none)"
  else if n < 12 then Printf.sprintf "(%d characters)" n
  else Printf.sprintf "...%s (%d characters)" (String.sub key (n - 4) 4) n

let is_token_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false

(* Prefixes of provider keys, assembled so that this file contains no
   key-shaped literal itself. *)
let key_prefixes = [ "sk" ^ "-"; "xai" ^ "-"; "sk" ^ "_" ]

let looks_like_key token =
  String.length token >= 24
  && String.for_all is_token_char token
  && List.exists (fun prefix -> Text.starts_with ~prefix token) key_prefixes
  && String.exists (function '0' .. '9' -> true | _ -> false) token

let tokens text =
  let acc = ref [] and current = Buffer.create 32 in
  let flush () =
    if Buffer.length current > 0 then begin
      acc := Buffer.contents current :: !acc;
      Buffer.clear current
    end
  in
  String.iter (fun c -> if is_token_char c then Buffer.add_char current c else flush ()) text;
  flush ();
  List.rev !acc

let find_key_shaped text = List.filter looks_like_key (tokens text)

let scrub ?(known = []) text =
  let secrets = List.filter (fun k -> String.length k >= 8) known @ find_key_shaped text in
  List.fold_left (fun acc secret -> Text.replace_all ~sub:secret ~by:"[redacted]" acc) text secrets

type status = { provider : string; variable : string; configured : bool; source : source option; shown : string }

let status ~getenv ~home (registry : Model_registry.t) =
  List.map
    (fun (p : Model_registry.provider) ->
      if p.Model_registry.key_env = "" then
        { provider = p.Model_registry.name; variable = ""; configured = true; source = None; shown = "(no key needed)" }
      else
        match lookup ~getenv ~home p with
        | Some (key, source) ->
          { provider = p.Model_registry.name; variable = p.Model_registry.key_env; configured = true; source = Some source; shown = redact key }
        | None ->
          { provider = p.Model_registry.name; variable = p.Model_registry.key_env; configured = false; source = None; shown = "(none)" })
    registry.Model_registry.providers
