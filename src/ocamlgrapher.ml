(** [main ()] is the entry point for ocamlgrapher. *)
let main () =
  match
    Config.from_cmdline (-10., 10.) (-10., 10.) 100 stdin Sys.argv
  with
  | Error e -> Printf.eprintf "%s\n" e
  | Ok cfg -> print_endline (Config.to_string cfg)

let () = main ()
