(** [main ()] is the entry point for ocamlgrapher. *)
let main () =
  let (Some cfg) = Config.from_args Sys.argv in
  print_endline (Config.to_string cfg)

let () = main ()
