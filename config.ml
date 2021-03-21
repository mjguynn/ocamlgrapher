(** Implementation of module [Config].*)

(* Unfortunately, I must re-implement this code because that's how OCaml
   works.*)
type command_t =
  | Graph
  | Points
  | Roots
  | Extrema

type t = { command : command_t }

let from_args : string array -> t option = failwith "Unimplemented"

let equation : t -> string = failwith "Unimplemented"

let domain : t -> float * float = failwith "Unimplemented"

let range : t -> float * float = failwith "Unimplemented"

let command : t -> command_t = failwith "Unimplemented"

let output_file : t -> string option = failwith "Unimplemented"

let to_string : t -> string = failwith "Unimplemented"
