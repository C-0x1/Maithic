(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

exception Failed of string
exception Skipped of string

type case = { name : string; body : unit -> unit }

let case name body = { name; body }
let fail msg = raise (Failed msg)
let skip reason = raise (Skipped reason)
let check what cond = if not cond then fail what

let equal ?show what expected actual =
  if expected <> actual then
    match show with
    | Some f -> fail (Printf.sprintf "%s\n  expected: %s\n  actual:   %s" what (f expected) (f actual))
    | None -> fail (what ^ ": values differ")

let equal_string what expected actual = equal ~show:(Printf.sprintf "%S") what expected actual
let equal_int what expected actual = equal ~show:string_of_int what expected actual
let equal_bool what expected actual = equal ~show:string_of_bool what expected actual

let equal_strings what expected actual =
  equal ~show:(fun l -> "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") l) ^ "]") what expected actual

let equal_ints what expected actual =
  equal ~show:(fun l -> "[" ^ String.concat "; " (List.map string_of_int l) ^ "]") what expected actual

let is_ok what = function Ok v -> v | Error e -> fail (Printf.sprintf "%s: unexpected error: %s" what e)
let is_error what = function Error e -> e | Ok _ -> fail (what ^ ": expected an error")

let contains what hay needle =
  if not (Text.contains hay needle) then fail (Printf.sprintf "%s\n  %S\n  does not contain %S" what hay needle)

let raises what f =
  match f () with
  | _ -> fail (what ^ ": expected an exception")
  | exception (Failed _ as e) -> raise e
  | exception _ -> ()

module Gen = struct
  type 'a t = Random.State.t -> 'a

  let size = ref 8
  let int lo hi st = if hi <= lo then lo else lo + Random.State.int st (hi - lo + 1)
  let bool st = Random.State.bool st
  let float lo hi st = lo +. Random.State.float st (hi -. lo)
  let char_of alphabet st = alphabet.[Random.State.int st (String.length alphabet)]
  let string_of ?(max = 12) alphabet st = String.init (int 0 max st) (fun _ -> char_of alphabet st)
  let one_of items st = List.nth items (Random.State.int st (List.length items))
  let choose gens st = (one_of gens st) st
  let map f g st = f (g st)
  let return x _ = x
  let pair a b st = let x = a st in (x, b st)
  let list ?(max = 6) g st = List.init (int 0 max st) (fun _ -> g st)

  let unicode_string ?(max = 10) () st =
    let pieces = [ "a"; "Z"; "0"; " "; "\""; "\\"; "\n"; "\t"; "/"; "\xC3\xA9"; "\xE2\x88\x80"; "\xE2\x89\xA4"; "\xF0\x9D\x94\xB8"; "{"; "]"; ":" ] in
    String.concat "" (List.init (int 0 max st) (fun _ -> one_of pieces st))

  let sized f st =
    let saved = !size in
    size := max 0 (saved / 2);
    let v = (try Ok (f saved st) with e -> Error e) in
    size := saved;
    match v with Ok v -> v | Error e -> raise e
end

let default_seed () =
  match Option.bind (Sys.getenv_opt "MAITHIC_TEST_SEED") int_of_string_opt with Some s -> s | None -> 20261122

let property ?(count = 200) ?show name gen law =
  case name (fun () ->
      let seed = default_seed () in
      let st = Random.State.make [| seed; Hashtbl.hash name |] in
      for i = 1 to count do
        let value = gen st in
        let holds = try law value with Failed m -> fail m | e -> fail ("exception: " ^ Printexc.to_string e) in
        if not holds then
          fail
            (Printf.sprintf "property failed on case %d of %d (seed %d)%s" i count seed
               (match show with Some f -> "\n  value: " ^ f value | None -> ""))
      done)

let temp_dir = Fs.temp_dir

let run suite groups =
  let filter = Sys.getenv_opt "MAITHIC_TEST_FILTER" in
  let selected group name =
    match filter with None | Some "" -> true | Some f -> Text.contains group f || Text.contains name f
  in
  let passed = ref 0 and failed = ref 0 and skipped = ref 0 in
  Printf.printf "suite %s\n%!" suite;
  List.iter
    (fun (group, cases) ->
      let cases = List.filter (fun c -> selected group c.name) cases in
      if cases <> [] then Printf.printf " %s\n%!" group;
      List.iter
        (fun c ->
          let started = Unix.gettimeofday () in
          let outcome =
            match c.body () with
            | () -> `Pass
            | exception Skipped reason -> `Skip reason
            | exception Failed msg -> `Fail msg
            | exception e -> `Fail ("exception: " ^ Printexc.to_string e)
          in
          let ms = (Unix.gettimeofday () -. started) *. 1000.0 in
          match outcome with
          | `Pass -> incr passed; Printf.printf "  ok    %s (%.1f ms)\n%!" c.name ms
          | `Skip reason -> incr skipped; Printf.printf "  skip  %s: %s\n%!" c.name reason
          | `Fail msg ->
            incr failed;
            Printf.printf "  FAIL  %s\n%s\n%!" c.name (Text.indent 8 msg))
        cases)
    groups;
  Printf.printf "%s: %d passed, %d failed, %d skipped\n%!" suite !passed !failed !skipped;
  if !failed > 0 then exit 1
