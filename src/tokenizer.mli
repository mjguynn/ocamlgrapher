(** Converts valid user-entered strings representing equations into
    tokens, according to the {{:../parsing.md} grammar rules}.*)

(** [tokenize equation_str] is a [function_type] carrying a tokenized
    list of the function [equation_str] passed into the function of
    [equation]. Raises: [Invalid_argument] if [equation] is not a
    grammatically valid mathematical function. *)
val tokenize : string -> Defs.function_type

(** [syntax_error error] raises an [Invalid_argument] exception
    representing a syntax error, and prints the [error] message. *)
val syntax_error : string -> 'a
