(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type output = { status : int; stdout : string; stderr : string }

let status_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

let merged_env extra =
  let current = Array.to_list (Unix.environment ()) in
  let key binding = match String.index_opt binding '=' with Some i -> String.sub binding 0 i | None -> binding in
  let kept = List.filter (fun b -> not (List.mem_assoc (key b) extra)) current in
  Array.of_list (kept @ List.map (fun (k, v) -> k ^ "=" ^ v) extra)

let find_in_path prog =
  if String.contains prog '/' then (if Sys.file_exists prog then Some prog else None)
  else
    let dirs = String.split_on_char ':' (Option.value (Sys.getenv_opt "PATH") ~default:"") in
    List.find_map
      (fun dir ->
        let candidate = Filename.concat (if dir = "" then "." else dir) prog in
        match Unix.access candidate [ Unix.X_OK ] with
        | () -> if Sys.is_directory candidate then None else Some candidate
        | exception Unix.Unix_error _ -> None)
      dirs

(* Starts [prog] with the given descriptors. The working directory is changed
   in a short shell-free wrapper: fork, chdir, exec. *)
let start ?(env = []) ?(inherit_env = true) ?cwd prog args ~stdin ~stdout ~stderr =
  match find_in_path prog with
  | None -> Error (Printf.sprintf "program not found: %s" prog)
  | Some exe ->
    let argv = Array.of_list (prog :: args) in
    let environment =
      if inherit_env then merged_env env else Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env)
    in
    (match cwd with
     | None ->
       (match Unix.create_process_env exe argv environment stdin stdout stderr with
        | pid -> Ok pid
        | exception Unix.Unix_error (err, _, _) ->
          Error (Printf.sprintf "cannot start %s: %s" prog (Unix.error_message err)))
     | Some dir ->
       if not (Sys.file_exists dir && Sys.is_directory dir) then
         Error (Printf.sprintf "working directory does not exist: %s" dir)
       else begin
         let exe = if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe in
         match Unix.fork () with
         | 0 ->
           (try
              Unix.chdir dir;
              Unix.dup2 stdin Unix.stdin;
              Unix.dup2 stdout Unix.stdout;
              Unix.dup2 stderr Unix.stderr;
              Unix.execve exe argv environment
            with _ -> Unix._exit 127)
         | pid -> Ok pid
         | exception Unix.Unix_error (err, _, _) ->
           Error (Printf.sprintf "cannot start %s: %s" prog (Unix.error_message err))
       end)

let close_quietly fd = try Unix.close fd with Unix.Unix_error _ -> ()

let run ?(stdin = "") ?env ?cwd ?timeout prog args =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  match start ?env ?cwd prog args ~stdin:in_r ~stdout:out_w ~stderr:err_w with
  | Error e ->
    List.iter close_quietly [ in_r; in_w; out_r; out_w; err_r; err_w ];
    Error e
  | Ok pid ->
    List.iter close_quietly [ in_r; out_w; err_w ];
    let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
    let out_buf = Buffer.create 4096 and err_buf = Buffer.create 1024 in
    let chunk = Bytes.create 65536 in
    let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
    let to_write = ref (if stdin = "" then None else Some 0) in
    if !to_write = None then close_quietly in_w;
    let readers = ref [ out_r; err_r ] in
    let timed_out = ref false in
    (try
       while !readers <> [] || !to_write <> None do
         let wait =
           match deadline with
           | None -> -1.0
           | Some d ->
             let left = d -. Unix.gettimeofday () in
             if left <= 0.0 then begin timed_out := true; raise Exit end;
             left
         in
         let writers = match !to_write with Some _ -> [ in_w ] | None -> [] in
         let ready_r, ready_w, _ =
           try Unix.select !readers writers [] wait with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
         in
         List.iter
           (fun fd ->
             let n = try Unix.read fd chunk 0 (Bytes.length chunk) with Unix.Unix_error _ -> 0 in
             if n = 0 then begin
               readers := List.filter (fun r -> r != fd) !readers;
               close_quietly fd
             end
             else Buffer.add_subbytes (if fd == out_r then out_buf else err_buf) chunk 0 n)
           ready_r;
         if ready_w <> [] then begin
           match !to_write with
           | None -> ()
           | Some offset ->
             let remaining = String.length stdin - offset in
             let n =
               try Unix.write_substring in_w stdin offset (min remaining 65536)
               with Unix.Unix_error _ -> remaining
             in
             if offset + n >= String.length stdin then begin
               to_write := None;
               close_quietly in_w
             end
             else to_write := Some (offset + n)
         end
       done
     with Exit -> ());
    Sys.set_signal Sys.sigpipe previous;
    if !timed_out then begin
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] pid);
      List.iter close_quietly (in_w :: !readers);
      Error (Printf.sprintf "%s did not finish within %.1f seconds" prog (Option.value timeout ~default:0.0))
    end
    else begin
      let _, status = Unix.waitpid [] pid in
      Ok { status = status_code status; stdout = Buffer.contents out_buf; stderr = Buffer.contents err_buf }
    end

type worker = {
  wpid : int;
  to_child : Unix.file_descr;
  from_child : Unix.file_descr;
  pending : Buffer.t;
  mutable closed : bool;
}

let spawn ?env ?inherit_env ?cwd prog args =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  match start ?env ?inherit_env ?cwd prog args ~stdin:in_r ~stdout:out_w ~stderr:Unix.stderr with
  | Error e ->
    List.iter close_quietly [ in_r; in_w; out_r; out_w ];
    Error e
  | Ok pid ->
    close_quietly in_r;
    close_quietly out_w;
    ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
    Ok { wpid = pid; to_child = in_w; from_child = out_r; pending = Buffer.create 4096; closed = false }

let pid w = w.wpid

let send_line w line =
  if w.closed then Error "worker is stopped"
  else begin
    let data = line ^ "\n" in
    let rec write offset =
      if offset < String.length data then
        write (offset + Unix.write_substring w.to_child data offset (String.length data - offset))
    in
    match write 0 with
    | () -> Ok ()
    | exception Unix.Unix_error (err, _, _) ->
      Error (Printf.sprintf "cannot write to worker: %s" (Unix.error_message err))
  end

(* Takes one complete line out of the pending buffer, if there is one. *)
let take_line w =
  let data = Buffer.contents w.pending in
  match String.index_opt data '\n' with
  | None -> None
  | Some i ->
    Buffer.clear w.pending;
    Buffer.add_substring w.pending data (i + 1) (String.length data - i - 1);
    let line = String.sub data 0 i in
    let n = String.length line in
    Some (if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line)

let read_line ?timeout w =
  if w.closed then Error "worker is stopped"
  else begin
    let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
    let chunk = Bytes.create 65536 in
    let rec loop () =
      match take_line w with
      | Some line -> Ok line
      | None ->
        let wait =
          match deadline with None -> -1.0 | Some d -> Float.max 0.0 (d -. Unix.gettimeofday ())
        in
        (match Unix.select [ w.from_child ] [] [] wait with
         | [], _, _ -> Error "timed out waiting for the worker"
         | _ ->
           (match Unix.read w.from_child chunk 0 (Bytes.length chunk) with
            | 0 -> Error "the worker closed its output"
            | n -> Buffer.add_subbytes w.pending chunk 0 n; loop ()
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
            | exception Unix.Unix_error (err, _, _) ->
              Error (Printf.sprintf "cannot read from worker: %s" (Unix.error_message err)))
         | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ())
    in
    loop ()
  end

let stop w =
  if w.closed then 0
  else begin
    w.closed <- true;
    close_quietly w.to_child;
    let rec wait attempts =
      match Unix.waitpid [ Unix.WNOHANG ] w.wpid with
      | 0, _ ->
        if attempts = 0 then begin
          (try Unix.kill w.wpid Sys.sigkill with Unix.Unix_error _ -> ());
          status_code (snd (Unix.waitpid [] w.wpid))
        end
        else begin
          Unix.sleepf 0.02;
          wait (attempts - 1)
        end
      | _, status -> status_code status
      | exception Unix.Unix_error _ -> 0
    in
    let code = wait 100 in
    close_quietly w.from_child;
    code
  end
