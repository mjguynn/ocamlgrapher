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

let syntax_error error =
  raise (Invalid_argument ("Syntax error: " ^ error))

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

let alpha_token_map =
  [
    ("sqrt", Function Sqrt);
    ("abs", Function Abs);
    ("ln", Function Ln);
    ("log", Function Log);
    ("pi", Constant Pi);
    ("sin", Function Sin);
    ("cos", Function Cos);
    ("tan", Function Tan);
    ("sec", Function Sec);
    ("csc", Function Csc);
    ("cot", Function Cot);
    ("arcsin", Function Arcsin);
    ("arccos", Function Arccos);
    ("arctan", Function Arctan);
    ("arcsec", Function Arcsec);
    ("arccsc", Function Arccsc);
    ("arccot", Function Arccot);
  ]

let is_numerical_subtoken ch =
  let code = Char.code ch in
  code = 46 || (code < 58 && code > 47)

let is_alpha_subtoken ch =
  let code = Char.code ch in
  code < 123 && code > 96

let is_starting_alpha_subtoken ch =
  List.mem ch [ 'p'; 's'; 'a'; 'l'; 'c'; 't' ]

let is_unit_token ch =
  List.mem ch [ '='; '+'; '-'; '*'; '/'; '^'; '('; ')'; 'x'; 'y'; 'e' ]

let extract_alpha_token (str : string) : token =
  match List.assoc_opt str alpha_token_map with
  | Some tok -> tok
  | None ->
      syntax_error
        "unknown character or symbol found. [alpha token assoc failure]"

(* if acc is num and cur is num add cur to acc if acc is num and cur is
   alpha add token, re-lex cur if acc is alpha and cur is alpha add cur
   to acc if acc is alpha and cur is num add token, re-lex cur *)

let tokenize equation_str =
  let tokens = ref [] in
  let add_token token = tokens := token :: !tokens in
  let rec lex tokens_str acc =
    if String.length tokens_str <> 0 then
      let hd = tokens_str.[0] in
      let tl = String.sub tokens_str 1 (String.length tokens_str - 1) in
      if String.length acc <> 0 then
        let starting_acc = acc.[0] in
        if is_numerical_subtoken hd then begin
          if is_numerical_subtoken starting_acc then
            lex tl (acc ^ Char.escaped hd)
          else if is_starting_alpha_subtoken starting_acc then begin
            add_token (extract_alpha_token acc);
            lex (Char.escaped hd ^ tl) ""
          end
        end
        else if is_alpha_subtoken hd then begin
          if is_numerical_subtoken starting_acc then begin
            add_token (Constant (Number (Float.of_string acc)));
            lex (Char.escaped hd ^ tl) ""
          end
          else if is_starting_alpha_subtoken starting_acc then
            lex tl (acc ^ Char.escaped hd)
        end
        else if is_unit_token hd then begin
          if is_numerical_subtoken starting_acc then begin
            add_token (Constant (Number (Float.of_string acc)));
            lex (Char.escaped hd ^ tl) ""
          end
          else if is_starting_alpha_subtoken starting_acc then begin
            add_token (extract_alpha_token acc);
            lex (Char.escaped hd ^ tl) ""
          end
        end
        else if hd = ' ' then lex tl acc
        else
          syntax_error
            "unknown character or symbol found. [unit subtoken assoc \
             failure]"
      else if is_numerical_subtoken hd || is_starting_alpha_subtoken hd
      then lex tl (acc ^ Char.escaped hd)
      else
        match List.assoc_opt hd unit_token_map with
        | Some tok ->
            add_token tok;
            lex tl ""
        | None ->
            if hd = ' ' then lex tl ""
            else
              syntax_error
                "unknown character or symbol found. [unit subtoken \
                 assoc failure]"
    else if String.length acc <> 0 then
      let starting_acc = acc.[0] in
      if is_numerical_subtoken starting_acc then
        add_token (Constant (Number (Float.of_string acc)))
      else if is_starting_alpha_subtoken starting_acc then
        add_token (extract_alpha_token acc)
  in
  lex equation_str "";
  List.rev !tokens
