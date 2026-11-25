(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** HTTP transport for live provider calls.

    Adapters build a {!request} and parse a {!response}; this module carries
    one to the provider and brings the other back. It runs the [curl]
    command, so the core needs no HTTP or TLS library. The API key never
    appears on a command line or in a file: headers travel to [curl] on its
    standard input, and the request body through a private temporary file. *)

open Maithic_base

type request = { meth : string; url : string; headers : (string * string) list; body : string }

type response = { status : int; headers : (string * string) list; body : string }

type t = ?timeout:float -> request -> (response, string) result
(** A transport. Tests substitute their own. *)

(* A value inside double quotes in a curl configuration file. *)
let quote value =
  let buf = Buffer.create (String.length value + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    value;
  Buffer.add_char buf '"';
  Buffer.contents buf

let curl_config ~timeout ~body_file ~header_file (req : request) =
  String.concat "\n"
    ([ "url = " ^ quote req.url;
       "request = " ^ quote req.meth;
       "silent";
       "show-error";
       "max-time = " ^ string_of_int (int_of_float (Float.ceil timeout));
       "dump-header = " ^ quote header_file;
       "write-out = " ^ quote "\\n%{http_code}" ]
     @ List.map (fun (k, v) -> "header = " ^ quote (k ^ ": " ^ v)) req.headers
     @ (if req.body = "" then [] else [ "data-binary = " ^ quote ("@" ^ body_file) ]))
  ^ "\n"

(* The header block of the final response: curl writes one block per
   response, so redirects and interim responses come first. *)
let parse_headers text =
  let blocks = Text.split_on "\r\n\r\n" text |> List.filter (fun b -> not (Text.is_blank b)) in
  match List.rev blocks with
  | [] -> []
  | last :: _ ->
    Text.split_on "\r\n" last
    |> List.filter_map (fun line ->
           match String.index_opt line ':' with
           | Some i when not (Text.starts_with ~prefix:"HTTP/" line) ->
             Some (String.lowercase_ascii (String.sub line 0 i), String.trim (String.sub line (i + 1) (String.length line - i - 1)))
           | _ -> None)

let header name (resp : response) = List.assoc_opt (String.lowercase_ascii name) resp.headers

let curl : t =
 fun ?(timeout = 120.0) req ->
  if Proc.find_in_path "curl" = None then Error "the curl command was not found on PATH"
  else begin
    let dir = Fs.temp_dir "maithic-http" in
    let body_file = Filename.concat dir "body" and header_file = Filename.concat dir "headers" in
    let cleanup () =
      Fs.remove body_file;
      Fs.remove header_file;
      (try Unix.rmdir dir with Unix.Unix_error _ -> ())
    in
    let result =
      match Fs.write body_file req.body with
      | Error e -> Error e
      | Ok () ->
        let config = curl_config ~timeout ~body_file ~header_file req in
        (match Proc.run ~stdin:config ~timeout:(timeout +. 10.0) "curl" [ "--config"; "-" ] with
         | Error e -> Error e
         | Ok out when out.Proc.status <> 0 ->
           Error (Printf.sprintf "curl exited with status %d: %s" out.Proc.status (String.trim out.Proc.stderr))
         | Ok out ->
           (match String.rindex_opt out.Proc.stdout '\n' with
            | None -> Error "curl produced no status line"
            | Some i ->
              let body = String.sub out.Proc.stdout 0 i in
              let code = String.trim (String.sub out.Proc.stdout (i + 1) (String.length out.Proc.stdout - i - 1)) in
              (match int_of_string_opt code with
               | None -> Error ("curl reported an unreadable status: " ^ code)
               | Some status ->
                 let headers = match Fs.read header_file with Ok text -> parse_headers text | Error _ -> [] in
                 Ok { status; headers; body })))
    in
    cleanup ();
    result
  end
