(** Implementation of module [Tokenizer]. *)

open Defs

let unit_token_map =
  [
    ('=', Operator Equals);
    ('+', Operator Plus);
    ('-', Operator Minus);
    ('*', Operator Times);
    ('/', Operator Divide);
    ('^', Operator Exponent);
    ('(', Parentheses LParen);
    (')', Parentheses RParen);
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

let syntax_error error =
  raise (Invalid_argument ("Syntax error: " ^ error))

let is_numerical_subtoken ch =
  let code = Char.code ch in
  code = 46 || (code < 58 && code > 47)

let is_alpha_subtoken ch =
  let code = Char.code ch in
  code < 123 && code > 96

let is_starting_alpha_subtoken ch =
  List.exists (function tok, _ -> tok.[0] = ch) alpha_token_map

let is_unit_token ch =
  List.exists (function tok, _ -> tok = ch) unit_token_map

let is_alpha_token ch =
  List.exists (function tok, _ -> tok = ch) alpha_token_map

let extract_alpha_token (str : string) : token =
  match List.assoc_opt str alpha_token_map with
  | Some tok -> tok
  | None ->
      syntax_error
        "unknown character or symbol found. [alpha token assoc failure]"

let should_accumulate cur acc =
  (is_numerical_subtoken cur && is_numerical_subtoken acc)
  || (is_alpha_subtoken cur && is_starting_alpha_subtoken acc)

let should_lex cur acc =
  (is_numerical_subtoken cur && is_starting_alpha_subtoken acc)
  || (is_alpha_subtoken cur && is_numerical_subtoken acc)
  || is_unit_token cur
     && (is_numerical_subtoken acc || is_starting_alpha_subtoken acc)

let tokenize equation_str =
  let tokens = ref [] in
  let add_token token = tokens := token :: !tokens in
  let rec lex tokens_str acc =
    if String.length tokens_str <> 0 then
      let hd = tokens_str.[0] in
      let tl = String.sub tokens_str 1 (String.length tokens_str - 1) in
      if hd <> ' ' then
        if String.length acc <> 0 then
          let starting_acc = acc.[0] in
          if is_alpha_token acc || should_lex hd starting_acc then begin
            if is_numerical_subtoken starting_acc then
              add_token (Constant (Number (Float.of_string acc)))
            else add_token (extract_alpha_token acc);
            lex (Char.escaped hd ^ tl) ""
          end
          else if should_accumulate hd starting_acc then
            lex tl (acc ^ Char.escaped hd)
          else
            syntax_error
              "unknown character or symbol found. [non-unit (sub)token \
               assoc failure]"
        else if
          is_numerical_subtoken hd || is_starting_alpha_subtoken hd
        then lex tl (Char.escaped hd)
        else
          match List.assoc_opt hd unit_token_map with
          | Some tok ->
              add_token tok;
              lex tl ""
          | None ->
              syntax_error
                "unknown character or symbol found. [unit token assoc \
                 failure]"
      else lex tl acc
    else if String.length acc <> 0 then
      let starting_acc = acc.[0] in
      if is_numerical_subtoken starting_acc then
        add_token (Constant (Number (Float.of_string acc)))
      else if is_starting_alpha_subtoken starting_acc then
        add_token (extract_alpha_token acc)
  in
  lex equation_str "";
  List.rev !tokens
