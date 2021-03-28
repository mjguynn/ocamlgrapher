(* Testing GitHub Desktop *)

type t

exception InvalidRange of string

val range_limiter : t -> 'a -> t

val root_estimator : t -> float list
