(** Implementation of module [Parser]. *)

open Defs

let get_tokens equation =
  match equation with
  | FunctionX lst | FunctionY lst -> Array.of_list lst
  | _ -> ( failwith "Impossible" [@coverage off] )

let get_function_type equation =
  match equation with
  | FunctionX _ -> true
  | FunctionY _ -> false
  | _ -> ( failwith "Impossible" [@coverage off] )

let string_token_map =
  List.merge
    (fun _ _ -> 0)
    (List.map (fun (k, v) -> (Char.escaped k, v)) Defs.unit_token_map)
    Defs.alpha_token_map

let string_of_token token =
  let rec get_key token list =
    match list with
    | (k, v) :: t -> if token = v then k else get_key token t
    | [] -> failwith "token not found"
  in
  match token with
  | Constant (Number n) -> "\"" ^ string_of_float n ^ "\""
  | EOF -> "end of input"
  | _ -> "\"" ^ get_key token string_token_map ^ "\""

let peek tokens index =
  if !index < Array.length tokens then tokens.(!index) else EOF

let peek_check token tokens index = token = peek tokens index

let next tokens index =
  if Bool.not (peek_check EOF tokens index) then index := !index + 1

let consume token tokens index =
  if peek_check token tokens index then next tokens index
  else
    Tokenizer.syntax_error
      ( "Expected " ^ string_of_token token ^ " but found "
      ^ string_of_token (peek tokens index) )

let get_f_output f expr =
  match f with
  | Ln -> log expr
  | Log -> log10 expr
  | Sqrt -> sqrt expr
  | Abs -> abs_float expr
  | Sin -> sin expr
  | Cos -> cos expr
  | Tan -> tan expr
  | Sec -> 1. /. cos expr
  | Csc -> 1. /. sin expr
  | Cot -> 1. /. tan expr
  | Arcsin -> asin expr
  | Arccos -> acos expr
  | Arctan -> atan expr
  | Arcsec -> acos (1. /. expr)
  | Arccsc -> asin (1. /. expr)
  | Arccot -> atan (1. /. expr)

let rec parse_elem tokens index input is_function_x =
  match peek tokens index with
  | Parentheses LParen ->
      next tokens index;
      let expr = parse_expr tokens index input is_function_x in
      consume (Parentheses RParen) tokens index;
      expr
  | Function f ->
      next tokens index;
      consume (Parentheses LParen) tokens index;
      let f_output =
        get_f_output f (parse_expr tokens index input is_function_x)
      in
      consume (Parentheses RParen) tokens index;
      f_output
  | Variable X | Variable Y ->
      if is_function_x then consume (Variable X) tokens index
      else consume (Variable Y) tokens index;
      input
  | Constant c -> (
      next tokens index;
      match c with
      | Pi -> Float.pi
      | E -> 2.71828182845904523
      | Number n -> n )
  | unknown ->
      Tokenizer.syntax_error
        ("Expected expression but found " ^ string_of_token unknown)

and parse_expr tokens index input is_function_x =
  let term = ref (parse_term tokens index input is_function_x) in
  while
    peek_check (Operator Plus) tokens index
    || peek_check (Operator Minus) tokens index
  do
    if peek_check (Operator Plus) tokens index then begin
      next tokens index;
      term := !term +. parse_term tokens index input is_function_x
    end
    else begin
      next tokens index;
      term := !term -. parse_term tokens index input is_function_x
    end
  done;
  !term

and parse_term tokens index input is_function_x =
  let factor = ref (parse_factor tokens index input is_function_x) in
  while
    peek_check (Operator Times) tokens index
    || peek_check (Operator Divide) tokens index
  do
    if peek_check (Operator Times) tokens index then begin
      next tokens index;
      factor := !factor *. parse_factor tokens index input is_function_x
    end
    else begin
      next tokens index;
      factor := !factor /. parse_factor tokens index input is_function_x
    end
  done;
  !factor

and parse_factor tokens index input is_function_x =
  if peek_check (Operator Minus) tokens index then begin
    next tokens index;
    -1. *. parse_factor tokens index input is_function_x
  end
  else parse_group tokens index input is_function_x

and parse_group tokens index input is_function_x =
  let elem = parse_elem tokens index input is_function_x in
  match peek tokens index with
  | Operator Exponent ->
      next tokens index;
      elem ** parse_elem tokens index input is_function_x
  | Operator _ | Parentheses RParen | EOF -> elem
  | _ -> elem *. parse_group tokens index input is_function_x

let compute_f equation input =
  let tokens = get_tokens equation in
  let is_function_x = get_function_type equation in
  let index = ref 0 in
  if
    peek_check
      (if is_function_x then Variable Y else Variable X)
      tokens index
  then begin
    next tokens index;
    consume (Operator Equals) tokens index;
    let expr = parse_expr tokens index input is_function_x in
    consume EOF tokens index;
    expr
  end
  else
    let expr = parse_expr tokens index input is_function_x in
    consume (Operator Equals) tokens index;
    consume
      (if is_function_x then Variable Y else Variable X)
      tokens index;
    consume EOF tokens index;
    expr
