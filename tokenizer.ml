(** Implementation of module [Tokenizer].*)

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

let is_alpha_subtoken ch = List.mem ch [ 'p'; 's'; 'a'; 'l'; 'c'; 't' ]

let tokenize str =
  let tokens = ref [] in
  let add_token token = tokens := token :: !tokens in
  let rec lex tokens_str =
    if String.length tokens_str <> 0 then
      let hd = tokens_str.[0] in
      let tl = String.sub tokens_str 1 (String.length tokens_str - 1) in
      if is_unit_token hd then begin
        if hd = '+' then add_token Plus
        else if hd = '-' then add_token Minus
        else if hd = '*' then add_token Times
        else if hd = '/' then add_token Divide
        else if hd = '^' then add_token Exponent
        else if hd = '(' then add_token LParen
        else if hd = ')' then add_token RParen
        else if hd = 'x' then add_token (Variable X)
        else if hd = 'y' then add_token (Variable Y)
        else if hd = 'e' then add_token (Constant E);
        lex tl
      end
      else if is_numerical_subtoken hd then
        let rec add_numerical_token num_str acc : string =
          if String.length num_str <> 0 then
            let num_hd = num_str.[0] in
            let num_tl =
              String.sub num_str 1 (String.length num_str - 1)
            in
            if is_numerical_subtoken num_hd then
              add_numerical_token num_tl (acc ^ Char.escaped num_hd)
            else begin
              add_token (Constant (Number (Float.of_string acc)));
              Char.escaped num_hd ^ num_tl
            end
          else begin
            add_token (Constant (Number (Float.of_string acc)));
            ""
          end
        in
        lex (add_numerical_token tl (Char.escaped hd))
      else if hd <> ' ' then raise (Invalid_argument "Syntax error")
  in
  lex str;
  List.rev !tokens
