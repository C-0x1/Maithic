(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type t = { clock : unit -> float; started : float; mutable round : int; mutable log : Council_message.t list (* the last message first *) }

let create ?(clock = Unix.gettimeofday) () = { clock; started = clock (); round = 0; log = [] }
let set_round t round = t.round <- round
let length t = List.length t.log
let messages t = List.rev t.log

let say t ~sender ~recipients performative ~subject ?(refers = []) content =
  let message =
    { Council_message.seq = length t + 1; at = Float.max 0.0 (t.clock () -. t.started); round = t.round; sender; recipients; performative; subject; content; refers }
  in
  match Council_message.check message with
  | Error why -> invalid_arg ("Council_transcript.say: " ^ why)
  | Ok () ->
    t.log <- message :: t.log;
    message

let from t member = List.filter (fun (m : Council_message.t) -> m.Council_message.sender = member) (messages t)
let to_ t member = List.filter (fun (m : Council_message.t) -> List.mem member m.Council_message.recipients) (messages t)
let to_json t = Json.List (List.map Council_message.to_json (messages t))

let of_json j =
  match j with
  | Json.List items ->
    let rec read expected acc = function
      | [] -> Ok (List.rev acc)
      | item :: rest ->
        (match Council_message.of_json item with
         | Error why -> Error (Printf.sprintf "message %d: %s" expected why)
         | Ok m when m.Council_message.seq <> expected -> Error (Printf.sprintf "message %d has the place %d: the log is not in order" expected m.Council_message.seq)
         | Ok m -> read (expected + 1) (m :: acc) rest)
    in
    read 1 [] items
  | _ -> Error "a transcript is an array of messages"

let to_markdown ?(title = "Council session") ?(items = fun _ -> None) t =
  let buffer = Buffer.create 2048 in
  let add = Buffer.add_string buffer in
  add (Printf.sprintf "# %s\n" title);
  let round = ref (-1) in
  List.iter
    (fun (m : Council_message.t) ->
      if m.Council_message.round <> !round then begin
        round := m.Council_message.round;
        add (if !round = 0 then "\n## Opening\n" else Printf.sprintf "\n## Round %d\n" !round)
      end;
      add
        (Printf.sprintf "\n**%d. %s → %s** · %s · %s\n\n" m.Council_message.seq m.Council_message.sender (String.concat ", " m.Council_message.recipients)
           (Council_message.performative_name m.Council_message.performative) m.Council_message.subject);
      (* What a member said is quoted, so that nothing in it can pass for a heading of the transcript. *)
      (match String.trim m.Council_message.content with
       | "" -> ()
       | content -> add (String.concat "\n" (List.map (fun line -> if line = "" then ">" else "> " ^ line) (String.split_on_char '\n' content)) ^ "\n"));
      match List.filter_map (fun id -> Option.map (fun name -> Printf.sprintf "#%d %s" id name) (items id)) m.Council_message.refers with
      | [] -> ()
      | named -> add (Printf.sprintf "\nRefers to: %s\n" (String.concat "; " named)))
    (messages t);
  Buffer.contents buffer
