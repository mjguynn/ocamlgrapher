open Tokenizer

let compute_f_of_x (equation : token list) (x : float) = 0.

let parse str =
  let tokens = Array.of_list (tokenize str) in

  let index = ref 0 in

  let peek () =
    if !index < Array.length tokens then tokens.(!index) else EOF
  in

  let peek_check token = token = peek () in

  let next () =
    if Bool.not (peek_check EOF) then begin
      index := !index + 1;
      tokens.(!index - 1)
    end
    else EOF
  in

  let consume token =
    if peek_check token then next ()
    else syntax_error "expected character not found. [consume failure]"
  in

  let rec parse_elem () =
    match peek () with
    | Operator LParen ->
        ignore (next ());
        parse_expr ();
        ignore (consume (Operator RParen))
    | Function _ ->
        ignore (next ());
        ignore (consume (Operator LParen));
        parse_expr ();
        ignore (consume (Operator RParen))
    | Variable X -> ignore (next ())
    | Constant _ -> ignore (next ())
    | _ ->
        syntax_error
          "expected character not found. [parse elem failure]"
  and parse_expr () =
    parse_term ();
    while peek_check (Operator Plus) || peek_check (Operator Minus) do
      ignore (next ());
      parse_term ()
    done
  and parse_term () =
    parse_factor ();
    while peek_check (Operator Times) || peek_check (Operator Divide) do
      ignore (next ());
      parse_factor ()
    done
  and parse_factor () =
    if peek_check (Operator Minus) then begin
      ignore (next ());
      parse_factor ()
    end
    else parse_group ()
  and parse_group () =
    parse_elem ();
    match peek () with
    | Operator Exponent ->
        ignore (next ());
        parse_elem ()
    | Operator _ -> ()
    | EOF -> ()
    | _ -> parse_group ()
  in

  let parse_equation () =
    if peek_check (Variable Y) then begin
      ignore (next ());
      ignore (consume (Operator Equals));
      parse_expr ()
    end
    else begin
      parse_expr ();
      ignore (consume (Operator Equals));
      ignore (consume (Variable Y))
    end;
    ignore (consume EOF)
  in

  parse_equation ()
