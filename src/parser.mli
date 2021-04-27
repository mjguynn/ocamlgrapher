(** Contains functions for evaluating a tokenized equation. *)

(** [compute_f_of_x equation x] is the numerical y-value output of [x]
    passed into the function of [equation]. Raises: [Invalid_argument]
    if [equation] is not a valid function. *)
val compute_f_of_x : Defs.token list -> float -> float
