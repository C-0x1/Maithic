(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The ways a statement fails to elaborate.

    A drafted statement that Lean rejects is rejected for one of a few
    reasons, and the reason says what a repair has to do. A name Lean does
    not know has to be replaced by what it stands for. An instance Lean
    cannot find means an operation is used on a type that does not have it.
    A type mismatch means two kinds of number met without a cast. An
    ambiguous notation needs a type written beside it. *)

type t =
  | Unknown_constant of string  (** A name that does not exist; the name. *)
  | Unknown_module of string  (** An import that is not there; the module. *)
  | Failed_instance  (** Lean could not find an instance of a type class. *)
  | Type_mismatch  (** A term does not have the type its place asks for. *)
  | Ambiguous  (** A notation has several readings and Lean could not choose. *)
  | Syntax  (** The text is not Lean. *)
  | Other

val name : t -> string
(** [unknown-constant], [unknown-module], [failed-instance], [type-mismatch], [ambiguous], [syntax] or [other]. *)

val names : string list

val classify : string -> t
(** The class of what Lean said. When Lean reported several errors, with an
    empty line between them, the class is that of the first. *)

val hint : t -> string
(** What a repair has to do for an error of the class, in a sentence. *)
