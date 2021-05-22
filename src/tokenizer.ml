(** Implementation of module [Tokenizer]. *)

open Defs

(** [unit_token_map] is a map which maps single-character tokens that
    are part of the equation grammar to their respective variant
    representations. *)
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

(** [alpha_token_map] is a map which maps multi-character tokens that
    are part of the equation grammar to their respective variant
    representations. *)
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

(** [is_numerical_subtoken ch] is [true] if [ch] is a numerical
    character, and [false] otherwise. *)
let is_numerical_subtoken ch =
  let code = Char.code ch in
  code = 46 || (code < 58 && code > 47)

(** [is_alpha_subtoken ch] is [true] if [ch] is a valid non-numerical
    character, and [false] otherwise. *)
let is_alpha_subtoken ch =
  let code = Char.code ch in
  code < 123 && code > 96

(** [is_starting_alpha_subtoken ch] is [true] if [ch] is a substring of
    a valid key contained in [alpha_token_map], and [false] otherwise. *)
let is_starting_alpha_subtoken ch =
  List.exists (function tok, _ -> tok.[0] = ch) alpha_token_map

(** [is_unit_token ch] is [true] if [ch] is a valid key contained in
    [unit_token_map], and [false] otherwise. *)
let is_unit_token ch =
  List.exists (function tok, _ -> tok = ch) unit_token_map

(** [is_alpha_token ch] is [true] if [ch] is a valid key contained in
    [alpha_token_map], and [false] otherwise. *)
let is_alpha_token ch =
  List.exists (function tok, _ -> tok = ch) alpha_token_map

(** [extract_alpha_token str] is the is the variant representation of
    the token [str]. *)
let extract_alpha_token str =
  match List.assoc_opt str alpha_token_map with
  | Some tok -> tok
  | None ->
      syntax_error
        "unknown character or symbol found. [alpha token assoc failure]"

(** [should_accumulate cur acc] is [true] if [cur] is a substring of a
    valid token that should be accumulated in order to tokenize later,
    and [false] otherwise. *)
let should_accumulate cur acc =
  (is_numerical_subtoken cur && is_numerical_subtoken acc)
  || (is_alpha_subtoken cur && is_starting_alpha_subtoken acc)

(** [should_lex cur acc] is [true] if [cur] is itself a valid token that
    should be tokenized into its variant representation, and [false]
    otherwise. *)
let should_lex cur acc =
  (is_numerical_subtoken cur && is_starting_alpha_subtoken acc)
  || (is_alpha_subtoken cur && is_numerical_subtoken acc)
  || is_unit_token cur
     && (is_numerical_subtoken acc || is_starting_alpha_subtoken acc)

(** [add_token token tokens] updated [tokens] with [token] appended to
    itself. *)
let add_token token tokens = tokens := token :: !tokens

(** [lex tokens_str acc tokens] lexes [tokens_str] into a list of token
    types contained in [tokens]. *)
let rec lex tokens_str acc tokens =
  if String.length tokens_str <> 0 then
    let hd = tokens_str.[0] in
    let tl = String.sub tokens_str 1 (String.length tokens_str - 1) in
    lex_non_empty acc hd tl tokens
  else if String.length acc <> 0 then
    let starting_acc = acc.[0] in
    if is_numerical_subtoken starting_acc then
      add_token (Constant (Number (Float.of_string acc))) tokens
    else if is_starting_alpha_subtoken starting_acc then
      add_token (extract_alpha_token acc) tokens

(** [update_tokens acc hd tl starting_acc tokens] adds the appropriate
    token type to [tokens] depending on what type of tokens [hd] and
    [tl] contain. *)
and update_tokens acc hd tl starting_acc tokens =
  if is_alpha_token acc || should_lex hd starting_acc then begin
    if is_numerical_subtoken starting_acc then
      add_token (Constant (Number (Float.of_string acc))) tokens
    else add_token (extract_alpha_token acc) tokens;
    lex (Char.escaped hd ^ tl) "" tokens
  end
  else if should_accumulate hd starting_acc then
    lex tl (acc ^ Char.escaped hd) tokens
  else
    syntax_error
      "unknown character or symbol found. [non-unit (sub)token assoc \
       failure]"

(** [lex_non_empty acc hd tl tokens] lexes [hd] and [tl] appropriately
    depending on what type of tokens they contain. *)
and lex_non_empty acc hd tl tokens =
  if hd <> ' ' then
    if String.length acc <> 0 then
      let starting_acc = acc.[0] in
      update_tokens acc hd tl starting_acc tokens
    else if is_numerical_subtoken hd || is_starting_alpha_subtoken hd
    then lex tl (Char.escaped hd) tokens
    else process_unit_token hd tl tokens
  else lex tl acc tokens

(** [process_unit_token hd tl tokens] matches [hd] to the map in
    [unit_token_map] and adds the corresponding variant token to
    [tokens]. Raises: [Invalid_argument] if [hd] is not in the map. *)
and process_unit_token hd tl tokens =
  match List.assoc_opt hd unit_token_map with
  | Some tok ->
      add_token tok tokens;
      lex tl "" tokens
  | None ->
      syntax_error
        "unknown character or symbol found. [unit token assoc failure]"

let reverse_token_list is_rev tokens =
  if is_rev then List.rev tokens else tokens

let determine_function_type is_rev tokens =
  match tokens with
  | h1 :: h2 :: t -> (
      match (h1, h2) with
      | Variable X, Operator Equals ->
          FunctionY (reverse_token_list is_rev tokens)
      | Variable Y, Operator Equals ->
          FunctionX (reverse_token_list is_rev tokens)
      | _ -> FunctionUnknown (reverse_token_list is_rev tokens))
  | _ -> failwith "impossible"

let tokenize equation_str =
  let tokens = ref [] in
  lex equation_str "" tokens;
  let function_type = determine_function_type true !tokens in
  match function_type with
  | FunctionUnknown _ ->
      determine_function_type false (List.rev !tokens)
  | _ -> function_type
