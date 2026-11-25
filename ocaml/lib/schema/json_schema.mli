(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** JSON Schema validation, for the documents that cross a language boundary.

    Every such document names its schema ([maithic.prove.run/1], ...) and the
    schemas themselves are files under [schemas/]. This module checks a
    document against a schema, so that what one language writes is what the
    others were promised.

    The validator implements the part of JSON Schema 2020-12 that the
    Maithic schemas use, and refuses the rest: a schema with a keyword it
    does not know is a schema with a mistake in it, and {!lint} says so.

    - any instance: [type], [enum], [const], [allOf], [anyOf], [oneOf],
      [not], [if]/[then]/[else], [$ref];
    - objects: [properties], [required], [additionalProperties],
      [minProperties];
    - arrays: [items], [prefixItems], [minItems], [maxItems], [uniqueItems];
    - strings: [minLength], [maxLength], [pattern];
    - numbers: [minimum], [maximum], [exclusiveMinimum],
      [exclusiveMaximum].

    A [$ref] is a pointer into the same file (["#/$defs/goal"]), another
    file relative to the referring one (["../common.schema.json"]), or both
    (["../common.schema.json#/$defs/status"]). The annotations [$schema],
    [$id], [$defs], [$comment], [title], [description], [examples] and
    [default] are accepted and have no effect.

    Patterns are regular expressions in the common subset of ECMA-262:
    literals, [.], classes with ranges and negation, [\d], [\w], [\s], the
    anchors [^] and [$], groups, alternation, and the quantifiers [*], [+],
    [?], [{n}], [{n,}] and [{n,m}]. They are matched byte by byte and, as
    JSON Schema says, anywhere in the string unless anchored. *)

open Maithic_base

type t
(** A schema, together with where it came from, so that references to
    neighbouring files resolve. *)

type error = {
  at : string;  (** where in the instance, as [$.results[0].status] *)
  message : string;
}

val of_json : ?dir:string -> Json.t -> t
(** A schema from a JSON value. References to other files are read relative
    to [dir], by default the current directory. *)

val load : string -> (t, string) result
(** Reads a schema file. *)

val validate : t -> Json.t -> error list
(** Every way in which the instance fails the schema, outermost first; the
    empty list when it conforms. *)

val conforms : t -> Json.t -> (unit, string) result
(** [Ok ()] when the instance conforms, otherwise the errors, one per line. *)

val lint : t -> error list
(** Mistakes in the schema itself: unknown keywords, keywords with values
    of the wrong shape, references that do not resolve, patterns that do not
    parse. [at] then points into the schema. A schema that lints clean never
    makes {!validate} fail for a reason of its own. *)

val error_to_string : error -> string

(** {1 Patterns} *)

val pattern_matches : string -> string -> (bool, string) result
(** [pattern_matches pattern text]: whether the pattern matches somewhere in
    the text, or why the pattern is not one this module reads. *)
