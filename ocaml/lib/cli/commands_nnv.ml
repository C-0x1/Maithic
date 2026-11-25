(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [nnv verify] command: a property of a neural network, decided by a
    verifier that is not trusted and checked by Lean's kernel. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_ledger
open Maithic_nnv
open Cli_args
open Commands

let nnv_fault number ~exit message = Fault.make ~area:"NNV" ~number ~exit message

(* A document of the schema that is expected, and the name it gives itself. *)
let named_document file schema what =
  let* text = read_source file in
  match Json.parse text with
  | Error e -> Error (nnv_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "%s is not %s: %s" file what e))
  | Ok json ->
    if Json.string_or "" "schema" json <> schema then Error (nnv_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "%s is not %s: its schema is not %s" file what schema))
    else Ok (match Json.string_or "" "name" json with "" -> Filename.remove_extension (Filename.basename file) | name -> name)

let verify_cmd =
  { path = [ "nnv"; "verify" ]; summary = "Decide a property of a neural network and have Lean's kernel check the answer."; synopsis = "MODEL PROPERTY";
    details =
      "MODEL is a ReLU network and PROPERTY an input box with the box its outputs are claimed to lie in, both as JSON with exact \
       numbers. The verifier, in Julia, computes bounds in floating point with outward rounding and answers one of three ways: the \
       property holds, with a box for every layer; it fails, with a point of the input box; or it cannot tell. The answer is a \
       claim. It is written as a Lean theorem about the exact network and proved by running a checker inside the kernel, a checker \
       that is itself proved sound. The status is verified or refuted only when the kernel accepted that theorem within the trust \
       policy; a claim that Lean does not accept is reported as failed, and settles nothing. The exit status is 0 for verified, 8 \
       for refuted and 7 otherwise.";
    options =
      [ value "max-corners" "N" "Corners of the input box the verifier may try as counterexamples (1024).";
        value "write" "FILE" "Write the Lean file with the theorem to FILE.";
        value "out" "FILE" "Write the job as JSON to FILE as well.";
        value "policy" "FILE" "A trust policy file." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* model, property =
              match parsed.positional with [ model; property ] -> Ok (model, property) | _ -> Error (usage_fault "nnv verify needs MODEL and PROPERTY")
            in
            let* model_name = named_document model "maithic.nnv.network/1" "a network" in
            let* property_name = named_document property "maithic.nnv.property/1" "a property" in
            let* max_corners =
              match get parsed "max-corners" with
              | None -> Ok 1024
              | Some t -> (match int_of_string_opt t with Some n when n >= 0 -> Ok n | _ -> Error (usage_fault ("--max-corners needs a whole number, 0 or more, and got " ^ t)))
            in
            let* policy = load_policy ctx in
            let project = Config.string ctx.config "nnv.project" in
            let* engine =
              Result.map_error
                (nnv_fault 1 ~exit:Exit_code.Toolchain)
                (Nnv_engine.locate ~julia:(Config.string ctx.config "oracle.julia")
                   ~project:(if project = "" then "" else Config.resolve_path ctx.config project)
                   ~cwd:(Sys.getcwd ()) ())
            in
            let* () = Result.map_error (nnv_fault 1 ~exit:Exit_code.Toolchain) (Nnv_engine.prepare engine) in
            let* located = locate_bridge ctx in
            let check ~source ~theorem =
              Result.map_error Bridge_error.to_string
                (Recheck.fresh ?cwd:located.Bridge_locate.cwd ~timeout:(Config.float ctx.config "bridge.timeout_s") ~limits:(limits ctx) ~argv:located.Bridge_locate.argv ~source theorem)
            in
            let verify = Nnv_engine.verify ~timeout:(Config.float ctx.config "nnv.timeout_s") ~max_corners engine ~model ~property in
            let* job = Result.map_error (nnv_fault 3 ~exit:Exit_code.Failure) (Nnv_job.run ~verify ~check ~policy ~model:model_name ~property:property_name) in
            let* () =
              match (get parsed "write", job.Nnv_job.lean) with
              | Some path, Some source -> Result.map_error usage_fault (Fs.write path source)
              | _ -> Ok ()
            in
            let document = Nnv_job.to_json job in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else begin
              print_endline (if ctx.quiet then Nnv_job.summary job else Nnv_job.render job);
              if not ctx.quiet then
                match (get parsed "write", job.Nnv_job.lean) with
                | Some path, Some _ -> print_endline (Term.dim (Printf.sprintf "wrote the theorem to %s; `maithic check %s` checks it again" path path))
                | Some _, None -> print_endline (Term.dim "nothing was written: there is no claim to state")
                | None, _ -> ()
            end;
            Ok (Exit_code.to_int (Nnv_job.exit_code job)))) }

let all = [ verify_cmd ]
