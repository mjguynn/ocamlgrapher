open Tokenizer

let compute_f_of_x equation x = 0

let parse str =
  let tokens = Array.of_list (tokenize str) in

  let index = ref 0 in

  let peek () =
    if !index < Array.length tokens then tokens.(!index) else EOF
  in

  let next () =
    index := !index + 1;
    if !index - 1 < Array.length tokens then tokens.(!index - 1)
    else EOF
  in

  let peek_check token = token = peek () in

  let consume token =
    if peek_check token then next () else syntax_error " (parser)"
  in

  let rec parse_elem () =
    match peek () with
    | LParen ->
        ignore (next ());
        parse_expr ();
        ignore (consume RParen)
    | Function _ ->
        ignore (next ());
        ignore (consume LParen);
        parse_expr ();
        ignore (consume RParen)
    | Variable _ -> ignore (next ())
    | Constant _ -> ignore (next ())
    | EOF -> ()
    | _ -> syntax_error "couldnt peek match elem"
  and parse_expr () =
    parse_term ();
    if peek_check (Operator Plus) || peek_check (Operator Minus) then begin
      ignore (next ());
      parse_term ()
    end
  and parse_term () =
    parse_factor ();
    if peek_check (Operator Times) || peek_check (Operator Divide) then begin
      ignore (next ());
      parse_factor ()
    end
  (* Implicit multiplication may not work *)
  and parse_factor () =
    parse_elem ();
    match peek () with
    | Operator Exponent ->
        ignore (next ());
        parse_elem ()
    | Operator _ -> ()
    | EOF -> ()
    | _ -> parse_factor ()
  in

  (* Chained addition, multiplication, subtraction, and division does
     not yet work *)
  let parse_equation () =
    parse_expr ();
    ignore (consume (Operator Equals));
    ignore (next ());
    parse_expr ()
  in

  parse_equation ()
