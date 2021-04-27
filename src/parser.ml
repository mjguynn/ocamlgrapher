(** Implementation of module [Parser]. *)

open Defs

let compute_f_of_x equation x =
  let tokens = Array.of_list equation in

  let index = ref 0 in

  let peek () =
    if !index < Array.length tokens then tokens.(!index) else EOF
  in

  let peek_check token = token = peek () in

  let next () = if Bool.not (peek_check EOF) then index := !index + 1 in

  let consume token =
    if peek_check token then next ()
    else
      Tokenizer.syntax_error
        "expected character not found. [consume failure]"
  in

  let rec parse_elem () =
    match peek () with
    | Parentheses LParen ->
        next ();
        let expr = parse_expr () in
        consume (Parentheses RParen);
        expr
    | Function f ->
        next ();
        consume (Parentheses LParen);
        let expr = parse_expr () in
        let f_output =
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
        in
        consume (Parentheses RParen);
        f_output
    | Variable X ->
        next ();
        x
    | Constant c -> (
        next ();
        match c with
        | Pi -> Float.pi
        | E -> 2.71828182845904523
        | Number n -> n )
    | _ ->
        Tokenizer.syntax_error
          "expected character not found. [parse elem failure]"
  and parse_expr () =
    let term = ref (parse_term ()) in
    while peek_check (Operator Plus) || peek_check (Operator Minus) do
      if peek_check (Operator Plus) then begin
        next ();
        term := !term +. parse_term ()
      end
      else begin
        next ();
        term := !term -. parse_term ()
      end
    done;
    !term
  and parse_term () =
    let factor = ref (parse_factor ()) in
    while peek_check (Operator Times) || peek_check (Operator Divide) do
      if peek_check (Operator Times) then begin
        next ();
        factor := !factor *. parse_factor ()
      end
      else begin
        next ();
        factor := !factor /. parse_factor ()
      end
    done;
    !factor
  and parse_factor () =
    if peek_check (Operator Minus) then begin
      next ();
      -1. *. parse_factor ()
    end
    else parse_group ()
  and parse_group () =
    let elem = parse_elem () in
    match peek () with
    | Operator Exponent ->
        next ();
        elem ** parse_elem ()
    | Operator _ | Parentheses RParen | EOF -> elem
    | _ -> elem *. parse_group ()
  in

  let parse_equation () =
    if peek_check (Variable Y) then begin
      next ();
      consume (Operator Equals);
      let expr = parse_expr () in
      consume EOF;
      expr
    end
    else
      let expr = parse_expr () in
      consume (Operator Equals);
      consume (Variable Y);
      consume EOF;
      expr
  in

  parse_equation ()
