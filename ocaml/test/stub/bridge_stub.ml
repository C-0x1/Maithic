(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* A stand-in for the Bridge that needs no Lean.

   It answers each request with the response recorded for the same method
   and parameters in a transcript file, one JSON object per line with
   members "request" and "response". The handshake is answered directly.
   Options make it misbehave on purpose, so that the client's handling of
   timeouts, protocol errors and incompatible servers can be tested:

     --hang-on METHOD       never answer this method
     --garbage-on METHOD    answer this method with text that is not JSON
     --wrong-id-on METHOD   answer this method with another identifier
     --protocol N           announce protocol version N in the handshake

   The extra method "stubEnvironment" returns the names of the environment
   variables the stub was started with. *)

open Maithic_base

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let transcript, options = match args with file :: rest -> (file, rest) | [] -> prerr_endline "usage: bridge_stub TRANSCRIPT [options]"; exit 2 in
  let rec option name = function
    | key :: value :: _ when key = name -> Some value
    | _ :: rest -> option name rest
    | [] -> None
  in
  let entries =
    match Fs.read transcript with
    | Error e -> prerr_endline e; exit 2
    | Ok text ->
      Text.lines text
      |> List.filter (fun line -> not (Text.is_blank line))
      |> List.filter_map (fun line ->
             match Json.parse line with
             | Ok j -> (match Json.member "request" j, Json.member "response" j with Some rq, Some rs -> Some (rq, rs) | _ -> None)
             | Error _ -> None)
  in
  let used = Array.make (List.length entries) false in
  let key request = (Json.member "method" request, Option.value (Json.member "params" request) ~default:(Json.Obj [])) in
  let with_id id response =
    match response with Json.Obj members -> Json.Obj (("id", id) :: List.remove_assoc "id" members) | other -> other
  in
  let reply j = print_endline (Json.to_string j) in
  let protocol = match Option.bind (option "--protocol" options) int_of_string_opt with Some n -> n | None -> Maithic_schema.Version.bridge_protocol in
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      (match Json.parse line with
       | Error _ -> reply (Json.Obj [ ("jsonrpc", Json.String "2.0"); ("id", Json.Null); ("error", Json.Obj [ ("code", Json.Int (-32700)); ("message", Json.String "parse error") ]) ])
       | Ok request ->
         let id = Option.value (Json.member "id" request) ~default:Json.Null in
         let meth = Json.string_or "" "method" request in
         let result fields = reply (Json.Obj [ ("jsonrpc", Json.String "2.0"); ("id", id); ("result", Json.Obj fields) ]) in
         if option "--hang-on" options = Some meth then Unix.sleep 3600
         else if option "--garbage-on" options = Some meth then print_endline "this is not JSON"
         else if option "--wrong-id-on" options = Some meth then
           reply (Json.Obj [ ("jsonrpc", Json.String "2.0"); ("id", Json.Int 987654); ("result", Json.Obj []) ])
         else if meth = "handshake" then
           result
             [ ("bridge", Json.String "stub"); ("protocol", Json.Int protocol); ("lean", Json.String Maithic_schema.Version.lean_toolchain);
               ("methods", Json.List [ Json.String "handshake" ]) ]
         else if meth = "stubEnvironment" then
           result
             [ ( "names",
                 Json.List
                   (Array.to_list (Unix.environment ())
                    |> List.map (fun b -> Json.String (match String.index_opt b '=' with Some i -> String.sub b 0 i | None -> b))) ) ]
         else if meth = "shutdown" then begin
           result [ ("bye", Json.Bool true) ];
           exit 0
         end
         else begin
           let wanted = key request in
           let rec find i = function
             | [] -> None
             | (rq, rs) :: rest ->
               if (not used.(i)) && fst (key rq) = fst wanted && Json.equal (snd (key rq)) (snd wanted) then begin
                 used.(i) <- true;
                 Some rs
               end
               else find (i + 1) rest
           in
           match find 0 entries with
           | Some response -> reply (with_id id response)
           | None ->
             reply
               (Json.Obj
                  [ ("jsonrpc", Json.String "2.0"); ("id", id);
                    ( "error",
                      Json.Obj
                        [ ("code", Json.Int (-32601)); ("message", Json.String ("the transcript has no answer for " ^ meth));
                          ("data", Json.Obj [ ("code", Json.String "MTH-BRG-0003") ]) ] ) ])
         end);
      loop ()
  in
  loop ()
