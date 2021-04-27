(** Implementation of module [Io]. *)

(** [style o s] returns the escape code for the virtual text formatting
    sequence [s] if [o] is a virtual terminal. Otherwise, it returns the
    empty string. Ex: [style "96" = "\x1b\[96m"]. *)
let style o s =
  if Unix.isatty (Unix.descr_of_out_channel o) then "\x1b[" ^ s ^ "m"
  else ""

let print_header ?channel:(c = stdout) s =
  Printf.fprintf c "%s%s%s%s" (style c "96") (style c "1") s
    (style c "0")

let print_detail ?channel:(c = stdout) s =
  Printf.fprintf c "%s%s%s" (style c "38;2;160;160;160") s (style c "0")

let print_error ?channel:(c = stderr) s =
  Printf.fprintf c "%s%s%s%s" (style stderr "91") (style stderr "1") s
    (style stderr "0")

let rec read_lines (ch : in_channel) : string list =
  let rec step (acc : string list) =
    try step (input_line ch :: acc) with End_of_file -> acc
  in
  step []
