open Tokenizer

let parse str =
  let tokens = Array.of_list (tokenize str) in

  let index = ref 0 in

  let peek () = tokens.(!index) in

  let next () =
    index := !index + 1;
    tokens.(!index - 1)
  in

  let peek_check token = token = peek () in

  let consume token =
    if peek_check token then next () else syntax_error " (parser)"
  in

  let rec parse_elem () =
    match peek () with
    | LParen ->
        parse_expr ();
        ignore (consume RParen)
    | Function _ ->
        ignore (next ());
        ignore (consume LParen);
        parse_expr ();
        ignore (consume RParen)
    | Variable _ -> ignore (next ())
    | Constant _ -> ignore (next ())
    | _ -> ()
  and parse_expr () =
    parse_term ();
    if peek_check Plus || peek_check Minus then begin
      ignore (next ());
      parse_term ()
    end
  and parse_term () =
    parse_factor ();
    if peek_check Times || peek_check Divide then begin
      ignore (next ());
      parse_factor ()
    end
  (* Implicit multiplication may not work *)
  and parse_factor () =
    parse_elem ();
    if peek_check Exponent then begin
      ignore (next ());
      parse_elem ()
    end
    else parse_factor ()
  in
  (* Chained addition, multiplication, subtraction, and division does
     not yet work *)
  let parse_equation () =
    parse_expr ();
    if peek_check Equals then begin
      ignore (next ());
      parse_expr ()
    end
  in

  parse_equation ()
