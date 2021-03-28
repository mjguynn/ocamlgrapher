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

type token =
  | Operator of operator
  | LParen
  | RParen
  | Constant of constant
  | Variable of variable
  | Function of m_function
  | EOF

val syntax_error : string -> 'a

val is_numerical_subtoken : char -> bool

val is_alpha_subtoken : char -> bool

val tokenize : string -> token list
