type variable

type constant

type m_function

type token

val is_unit_token : char -> bool

val is_numerical_subtoken : char -> bool

(* val is_alpha_subtoken : char -> bool *)

val explode : string -> char list

val tokenize : string -> token list
