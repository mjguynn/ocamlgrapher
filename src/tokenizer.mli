(** [variable] represents a variable, as specified in the grammar. *)
type variable =
  | X
  | Y

(** [constant] represents a constant value, as specified in the grammar. *)
type constant =
  | E
  | Pi
  | Number of float

(** [m_function] represents a mathematical function, as specified in the
    grammar. *)
type m_function =
  | Sqrt
  | Abs
  | Ln
  | Log
  | Sin
  | Cos
  | Tan
  | Cot
  | Sec
  | Csc
  | Arcsin
  | Arccos
  | Arctan
  | Arccot
  | Arcsec
  | Arccsc

(** [operator] represents a mathematical operator token, as specified in
    the grammar. *)
type operator =
  | Equals
  | Plus
  | Minus
  | Times
  | Divide
  | Exponent

(** [parentheses] represents a a left or right parentheses, as specified
    in the grammar. *)
type parentheses =
  | LParen
  | RParen

(** [token] represents a token, or terminal symbol of a grammatically
    valid mathematical function. *)
type token =
  | Operator of operator
  | Parentheses of parentheses
  | Constant of constant
  | Variable of variable
  | Function of m_function
  | EOF

(** [tokenize equation_str] is a tokenized list of the function
    [equation_str] passed into the function of [equation]. Raises:
    [Invalid_argument] if [equation] is not a grammatically valid
    mathematical function. *)
val tokenize : string -> token list

(** [syntax_error error] raises an [Invalid_argument] exception
    representing a syntax error, and prints the [error] message. *)
val syntax_error : string -> 'a
