(** [token] represents a token, or terminal symbol of a grammatically
    valid mathematical function. *)
type token

(** [tokenize equation_str] is a tokenized list of the function
    [equation_str] passed into the function of [equation]. Raises:
    [Invalid_argument] if [equation] is not a grammatically valid
    mathematical function. *)
val tokenize : string -> token list

(** [syntax_error error] raises an [Invalid_argument] exception
    representing a syntax error, and prints the [error] message. *)
val syntax_error : string -> 'a
