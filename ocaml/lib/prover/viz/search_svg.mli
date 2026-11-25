(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A picture of a proof search.

    Every goal is a card: its hypotheses, what is to be shown, and the
    tactics that failed on it or led in a circle. The colour of a card is its
    status. A tactic that left goals is a label under the card with a line to
    each goal it left, and the tactics of the proof that was found are drawn
    in the colour of proved goals, so that the proof can be read off the
    picture from the top down.

    The picture depends on the record alone and not on its timings: equal
    searches give equal pictures, byte for byte. *)

val render : ?max_nodes:int -> Search_record.t -> string
(** An SVG document. At most [max_nodes] goals are drawn, sixty unless
    stated; a tactic whose goals no longer fit says how many it left. *)
