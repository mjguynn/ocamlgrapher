open Tokenizer

(** [compute_f_of_x equation x] is the numerical y-value output of [x]
    passed into the function of [equation]. Raises: [Invalid_argument]
    if [equation] is not a valid function and [Not_found] if function is
    discontinuous at [x]. *)
val compute_f_of_x : token list -> float -> float

val parse : string -> unit
