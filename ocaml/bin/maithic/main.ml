(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

let () = exit (Maithic_cli.Main_cli.run (List.tl (Array.to_list Sys.argv)))
