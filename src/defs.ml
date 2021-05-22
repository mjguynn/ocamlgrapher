(** Contains definitions for types which would otherwise need to be
    duplicated between ML and MLI files.*)

(** [variable] represents a mathematical variable. *)
type variable =
  | X
  | Y

(** [constant] represents a constant value. *)
type constant =
  | E
  | Pi
  | Number of float

(** [m_function] represents a mathematical function. *)
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

(** [operator] represents a mathematical operator token. *)
type operator =
  | Equals
  | Plus
  | Minus
  | Times
  | Divide
  | Exponent

(** [parentheses] represents a left or right parentheses. *)
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

(** [function_type] is either a function in terms of x, [FunctionX], a
    function in terms of y, [FunctionY], or an unknown function
    [FunctionUnknown], and carries the corresponding tokenized equation. *)
type function_type =
  | FunctionX of token list
  | FunctionY of token list
  | FunctionUnknown of token list

(** [parse_rule] represents a rule that should be respected while
    parsing. A [Flag] rule represents an option that takes no
    parameters. A [Opt] rule represents an option that takes no
    parameters, aka, a "parameterized option". The first element in the
    parse rule is a non-zero string representing the GNU long option
    name of the option, and the second element is an optional character
    that can be used as a short option. For example, if you wanted to
    use "-h" or "--help", you would add an [Opt ("help", Some 'h')].*)
type parse_rule =
  | Flag of (string * char option)
  | Opt of (string * char option)

(** [command] represents the "action" that the user wants the program to
    perform on their provided equation. [Graph] means to graph the
    equation. [Points] means to calculate a list of points satisfying
    the equation. [Roots] means to calculate a list of roots of the
    equation. [Extrema] means to calculate a list of extrema of the
    equation. *)
type command =
  | Graph
  | Points
  | Roots
  | Extrema
