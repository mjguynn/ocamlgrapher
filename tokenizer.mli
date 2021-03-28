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

type token =
  | Equals
  | Plus
  | Minus
  | Times
  | Divide
  | Exponent
  | LParen
  | RParen
  | Constant of constant
  | Variable of variable
  | Function of m_function

val syntax_error : string -> 'a

val is_unit_token : char -> bool

val is_numerical_subtoken : char -> bool

val is_alpha_subtoken : char -> bool

val tokenize : string -> token list
