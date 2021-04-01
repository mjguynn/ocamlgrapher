(** Implementation of module [Tokenizer]. *)

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

let unit_token_map =
  [
    ('=', Operator Equals);
    ('+', Operator Plus);
    ('-', Operator Minus);
    ('*', Operator Times);
    ('/', Operator Divide);
    ('^', Operator Exponent);
    ('(', Operator LParen);
    (')', Operator RParen);
    ('x', Variable X);
    ('y', Variable Y);
    ('e', Constant E);
  ]

let is_numerical_subtoken ch =
  let code = Char.code ch in
  code = 46 || (code < 58 && code > 47)

let is_alpha_subtoken ch = List.mem ch [ 'p'; 's'; 'a'; 'l'; 'c'; 't' ]

let syntax_error error =
  raise (Invalid_argument ("Syntax error: " ^ error))

let tokenize equation_str =
  let tokens = ref [] in
  let add_token token = tokens := token :: !tokens in
  let rec lex tokens_str acc =
    if String.length tokens_str <> 0 then
      let hd = tokens_str.[0] in
      let tl = String.sub tokens_str 1 (String.length tokens_str - 1) in
      if is_numerical_subtoken hd then lex tl (acc ^ Char.escaped hd)
      else if String.length acc <> 0 then begin
        add_token (Constant (Number (Float.of_string acc)));
        lex (Char.escaped hd ^ tl) ""
      end
      else
        match List.assoc_opt hd unit_token_map with
        | Some tok ->
            add_token tok;
            lex tl ""
        | None ->
            if hd = ' ' then lex tl ""
            else if is_alpha_subtoken hd then
              match hd with
              | 'p' ->
                  if String.sub tl 0 1 = "i" then begin
                    add_token (Constant Pi);
                    lex (String.sub tl 1 (String.length tl - 1)) ""
                  end
                  else syntax_error "1"
              | 's' -> (
                  match String.sub tl 0 2 with
                  | "in" ->
                      add_token (Function Sin);
                      lex (String.sub tl 2 (String.length tl - 2)) ""
                  | "ec" ->
                      add_token (Function Sec);
                      lex (String.sub tl 2 (String.length tl - 2)) ""
                  | "qr" ->
                      if String.sub tl 2 1 = "t" then begin
                        add_token (Function Sqrt);
                        lex (String.sub tl 3 (String.length tl - 3)) ""
                      end
                      else syntax_error "2"
                  | _ -> syntax_error "3")
              | 'a' -> (
                  match String.sub tl 0 2 with
                  | "bs" ->
                      add_token (Function Abs);
                      lex (String.sub tl 2 (String.length tl - 2)) ""
                  | "rc" ->
                      begin
                        match String.sub tl 2 3 with
                        | "sin" -> add_token (Function Arcsin)
                        | "cos" -> add_token (Function Arccos)
                        | "tan" -> add_token (Function Arctan)
                        | "cot" -> add_token (Function Arccot)
                        | "sec" -> add_token (Function Arcsec)
                        | "csc" -> add_token (Function Arccsc)
                        | _ -> syntax_error "4"
                      end;
                      lex (String.sub tl 5 (String.length tl - 5)) ""
                  | _ -> syntax_error "5")
              | 'l' -> (
                  match String.sub tl 0 1 with
                  | "n" ->
                      add_token (Function Ln);
                      lex (String.sub tl 1 (String.length tl - 1)) ""
                  | "o" ->
                      if String.sub tl 1 1 = "g" then begin
                        add_token (Function Log);
                        lex (String.sub tl 2 (String.length tl - 2)) ""
                      end
                      else syntax_error "6"
                  | _ -> syntax_error "7")
              | 'c' ->
                  begin
                    match String.sub tl 0 2 with
                    | "os" -> add_token (Function Cos)
                    | "sc" -> add_token (Function Csc)
                    | _ -> syntax_error "8"
                  end;
                  lex (String.sub tl 2 (String.length tl - 2)) ""
              | 't' ->
                  if String.sub tl 0 2 = "an" then begin
                    add_token (Function Tan);
                    lex (String.sub tl 2 (String.length tl - 2)) ""
                  end
                  else syntax_error "9"
              | _ -> ()
            else syntax_error "10"
    else if String.length acc <> 0 then
      add_token (Constant (Number (Float.of_string acc)))
  in
  lex equation_str "";
  List.rev !tokens
