type variable =
  | X
  | Y

type constant =
  | E
  | Pi
  | Number of float

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

type operator =
  | Equals
  | Plus
  | Minus
  | Times
  | Divide
  | Exponent
  | LParen
  | RParen

type token =
  | Operator of operator
  | Constant of constant
  | Variable of variable
  | Function of m_function
  | EOF

val syntax_error : string -> 'a

val is_numerical_subtoken : char -> bool

val is_alpha_subtoken : char -> bool

(** [tokenize equation_str ] is a tokenized list of the function
    [equation_str] passed into the function of [equation]. Raises:
    [Invalid_argument] if [equation] is not a valid function. *)
val tokenize : string -> token list
