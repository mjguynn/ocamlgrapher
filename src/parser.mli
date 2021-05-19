(** Contains functions for evaluating a tokenized equation. *)

(** [compute_f equation input] is the numerical y-value output of
    [input] passed into the function of [equation]. Raises:
    [Invalid_argument] if [equation] is not a valid function. *)
val compute_f : Defs.function_type -> float -> float
