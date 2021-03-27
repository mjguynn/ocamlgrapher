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

let is_unit_token ch =
  List.mem ch [ '+'; '-'; '*'; '/'; '^'; '('; ')'; 'x'; 'y'; 'e' ]

let is_numerical_subtoken ch =
  let code = Char.code ch in
  code = 46 || (code < 58 && code > 47)

let is_alpha_subtoken ch = failwith "Unimplemented"

let explode str = List.init (String.length str) (String.get str)

let tokenize str =
  let tokens = ref [] in
  let add_token token = tokens := token :: !tokens in
  let rec lex char_lst =
    match char_lst with
    | h :: t ->
        if is_unit_token h then begin
          if h = '+' then add_token Plus
          else if h = '-' then add_token Minus
          else if h = '*' then add_token Times
          else if h = '/' then add_token Divide
          else if h = '^' then add_token Exponent
          else if h = '(' then add_token LParen
          else if h = ')' then add_token RParen
          else if h = 'x' then add_token (Variable X)
          else if h = 'y' then add_token (Variable Y)
          else if h = 'e' then add_token (Constant E);
          lex t
        end
        else if is_numerical_subtoken h then
          let rec add_numerical_token lst acc : char list =
            match lst with
            | hd :: tl ->
                if is_numerical_subtoken hd then
                  add_numerical_token tl (acc ^ Char.escaped hd)
                else begin
                  add_token (Constant (Number (Float.of_string acc)));
                  hd :: tl
                end
            | [] ->
                add_token (Constant (Number (Float.of_string acc)));
                []
          in
          lex (add_numerical_token t (Char.escaped h))
        else if h <> ' ' then raise (Invalid_argument "Syntax error")
    | [] -> ()
  in
  lex (explode str);
  List.rev !tokens
