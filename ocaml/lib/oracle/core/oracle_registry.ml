(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

module type ORACLE = sig
  val name : string
  val description : string
  val kinds : Oracle_goal.kind list
  val domains : Oracle_goal.domain list
  val solve : ?timeout:float -> Oracle_goal.t -> (Oracle_outcome.t, string) result
end

type t = (module ORACLE) list

let name_of (module O : ORACLE) = O.name
let create oracles = List.fold_left (fun kept o -> List.filter (fun k -> name_of k <> name_of o) kept @ [ o ]) [] oracles
let names t = List.map name_of t
let all t = t
let find t name = List.find_opt (fun o -> name_of o = name) t
let accepts (module O : ORACLE) (g : Oracle_goal.t) = List.mem g.Oracle_goal.kind O.kinds && List.mem g.Oracle_goal.domain O.domains
let for_goal t g = List.filter (fun o -> accepts o g) t
let default_timeout = 60.0

let solve ?timeout t goal =
  match for_goal t goal with
  | [] ->
    Error
      (Printf.sprintf "no oracle takes goals of kind %s over %s" (Oracle_goal.kind_to_string goal.Oracle_goal.kind)
         (Oracle_goal.domain_to_string goal.Oracle_goal.domain))
  | (module O : ORACLE) :: _ ->
    let started = Unix.gettimeofday () in
    Result.map
      (fun outcome -> { Oracle_outcome.oracle = O.name; goal; outcome; seconds = Unix.gettimeofday () -. started })
      (O.solve ?timeout goal)

type engine = Julia_package.t = { julia : string; project : string }

let locate ?julia ?project ~cwd () = Julia_package.locate ~package:"MaithicOracle" ~hint:"set oracle.project" ?julia ?project ~cwd ()

let command ?(quick = false) engine =
  ( engine.julia,
    [ "--startup-file=no" ]
    @ (if quick then [ "-O0"; "--compile=min" ] else [])
    @ [ "--project=" ^ engine.project; Julia_package.script engine "solve.jl" ] )

let quick_seconds = 10.0
let setup_command = Julia_package.setup_command
let prepare = Julia_package.prepare

let run ?(timeout = default_timeout) ~runner goal =
  match runner ?timeout:(Some timeout) ~stdin:(Json.to_string (Oracle_goal.to_json goal)) () with
  | Error e -> Error ("the oracle did not answer: " ^ e)
  | Ok (out : Proc.output) ->
    if out.Proc.status <> 0 then
      Error (Printf.sprintf "the oracle stopped with status %d: %s" out.Proc.status (Text.truncate 400 (String.trim out.Proc.stderr)))
    else
      (match Json.parse out.Proc.stdout with
       | Error e -> Error ("the oracle wrote something that is not JSON: " ^ e)
       | Ok j -> Oracle_outcome.of_json j)

let julia_oracle engine ~name ~description ~kinds ~domains : (module ORACLE) =
  (module struct
    let name = name
    let description = description
    let kinds = kinds
    let domains = domains

    let solve ?(timeout = default_timeout) goal =
      (* Nothing is fetched: the package depends on the standard library and on its sibling. *)
      let runner ~quick ?timeout ~stdin () =
        let program, args = command ~quick engine in
        Proc.run ~stdin ~env:Julia_package.offline ?timeout program args
      in
      (* Most goals are small, and for a small goal compiling the engine takes far longer than
         running it. The first try runs the engine without compiling, for a short while; a goal
         that needs more is run again, compiled, with the whole time. *)
      match prepare engine with
      | Error e -> Error e
      | Ok () ->
        (match run ~timeout:(Float.min timeout quick_seconds) ~runner:(runner ~quick:true) goal with
         | Ok outcome -> Ok outcome
         | Error _ when timeout > quick_seconds -> run ~timeout ~runner:(runner ~quick:false) goal
         | Error e -> Error e)
  end)

let builtin engine =
  let open Oracle_goal in
  create
    [ julia_oracle engine ~name:"linear"
        ~description:"Linear arithmetic by exact simplex: Farkas multipliers, or an exact counterexample." ~kinds:[ Linear ]
        ~domains:[ Integers; Rationals ];
      julia_oracle engine ~name:"polynomial"
        ~description:"Polynomial equations by Buchberger's algorithm: cofactors, or a point where the goal fails."
        ~kinds:[ Polynomial ] ~domains:[ Integers; Rationals ];
      julia_oracle engine ~name:"real"
        ~description:"Real inequalities on a box by interval arithmetic: boxes with enclosures, or a point where the goal fails."
        ~kinds:[ Real ] ~domains:[ Reals ] ]
