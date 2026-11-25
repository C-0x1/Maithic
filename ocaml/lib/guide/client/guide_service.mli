(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The guidance service as a process.

    The service is the entry point [bin/serve.jl] of the Julia package
    [MaithicGuide], started on the file of a trained model. It stays up for
    the length of a search and answers a request in well under a
    millisecond of computing; what takes time is starting Julia, once. *)

open Maithic_base

val locate : ?julia:string -> ?project:string -> cwd:string -> unit -> (Julia_package.t, string) result
(** Finds the Julia executable and the package [MaithicGuide] in the source
    tree around [cwd], unless they are given. *)

val command : Julia_package.t -> model:string -> string * string list
(** The program and arguments that serve a model. *)

val train_command : Julia_package.t -> dataset:string -> out:string -> string list -> string * string list
(** The program and arguments that train a model on a dataset directory and
    write it, with the record of the run, to [out]. The list holds further
    options of the training script, as given. *)

val default_startup : float
(** How long the service may take to greet: two minutes. The first start on
    a machine compiles the package. *)

val default_timeout : float
(** How long one answer may take: thirty seconds. *)

val start : ?startup:float -> ?timeout:float -> Julia_package.t -> model:string -> (Guide_client.transport, string) result
(** Starts the service on a model file. The process is spoken to line by
    line; the first answer may take [startup] seconds and every later one
    [timeout]. A file that is not there is an error before anything is
    started. *)

val connect : ?startup:float -> ?timeout:float -> Julia_package.t -> model:string -> (Guide_client.t, string) result
(** Makes the package ready, starts the service and greets it. *)
