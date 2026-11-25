(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

let ( / ) a b =
  if a = "" || a = "." then b
  else if b = "" then a
  else if a.[String.length a - 1] = '/' then a ^ b
  else a ^ "/" ^ b

let basename = Filename.basename
let dirname = Filename.dirname
let extension path = String.lowercase_ascii (Filename.extension path)

let read path =
  match In_channel.with_open_bin path In_channel.input_all with
  | contents -> Ok contents
  | exception Sys_error e -> Error e

let exists path = Sys.file_exists path
let is_dir path = Sys.file_exists path && Sys.is_directory path

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || is_dir dir then Ok ()
  else
    match mkdir_p (Filename.dirname dir) with
    | Error _ as e -> e
    | Ok () ->
      (match Unix.mkdir dir 0o755 with
       | () -> Ok ()
       | exception Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
       | exception Unix.Unix_error (err, _, _) ->
         Error (Printf.sprintf "cannot create directory %s: %s" dir (Unix.error_message err)))

let write path contents =
  match mkdir_p (Filename.dirname path) with
  | Error _ as e -> e
  | Ok () ->
    let tmp = Printf.sprintf "%s.tmp.%d" path (Unix.getpid ()) in
    (match
       Out_channel.with_open_bin tmp (fun oc -> Out_channel.output_string oc contents);
       Sys.rename tmp path
     with
     | () -> Ok ()
     | exception Sys_error e ->
       (try Sys.remove tmp with Sys_error _ -> ());
       Error e)

let list_dir dir =
  match Sys.readdir dir with
  | entries ->
    let entries = Array.to_list entries in
    List.sort compare entries
  | exception Sys_error _ -> []

let walk ?(skip = fun _ -> false) root =
  let rec go rel acc =
    let abs = if rel = "" then root else root / rel in
    List.fold_left
      (fun acc name ->
        let rel' = if rel = "" then name else rel / name in
        if skip rel' then acc
        else
          match (Unix.lstat (root / rel')).Unix.st_kind with
          | Unix.S_DIR -> go rel' acc
          | Unix.S_REG -> rel' :: acc
          | _ -> acc
          | exception Unix.Unix_error _ -> acc)
      acc (list_dir abs)
  in
  List.sort compare (go "" [])

let remove path = try Sys.remove path with Sys_error _ -> ()

let temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let rec attempt n =
    let dir = Filename.concat base (Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) n) in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (n + 1)
  in
  attempt 0

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    List.iter (fun name -> remove_tree (path / name)) (list_dir path);
    (try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> remove path
  | exception Unix.Unix_error _ -> ()
