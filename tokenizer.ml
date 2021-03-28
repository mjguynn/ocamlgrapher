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

let syntax_error tst = raise (Invalid_argument ("Syntax error" ^ tst))

let tokenize str =
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
      else if is_unit_token hd then begin
        begin
          match hd with
          | '+' -> add_token Plus
          | '-' -> add_token Minus
          | '*' -> add_token Times
          | '/' -> add_token Divide
          | '^' -> add_token Exponent
          | '(' -> add_token LParen
          | ')' -> add_token RParen
          | 'x' -> add_token (Variable X)
          | 'y' -> add_token (Variable Y)
          | 'e' -> add_token (Constant E)
          | _ -> ()
        end;
        lex tl ""
      end
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
      else if hd == ' ' then lex tl ""
      else syntax_error "10"
    else if String.length acc <> 0 then
      add_token (Constant (Number (Float.of_string acc)))
  in
  lex str "";
  List.rev !tokens
