(** Implementation of module [Parser]. *)

open Defs

let compute_f equation input =
  let latex = ref "" in

  let get_latex = false in

  let append_latex str = if get_latex then latex := !latex ^ str in

  let tokens =
    match equation with
    | FunctionX lst | FunctionY lst -> Array.of_list lst
    | _ -> failwith "impossible"
  in

  let is_function_x =
    match equation with
    | FunctionX _ -> true
    | FunctionY _ -> false
    | _ -> failwith "impossible"
  in

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
    let abs = ref false in
    match peek () with
    | Parentheses LParen ->
        next ();
        append_latex " \\left(";
        let expr = parse_expr () in
        consume (Parentheses RParen);
        append_latex " \\right)";
        expr
    | Function f ->
        next ();
        consume (Parentheses LParen);
        let f_output =
          match f with
          | Ln ->
              append_latex " \\ln{\\left(";
              log (parse_expr ())
          | Log ->
              append_latex " \\log{\\left(";
              log10 (parse_expr ())
          | Sqrt ->
              append_latex " \\sqrt{\\left(";
              sqrt (parse_expr ())
          | Abs ->
              append_latex " \\left|";
              abs := true;
              abs_float (parse_expr ())
          | Sin ->
              append_latex " \\sin{\\left(";
              sin (parse_expr ())
          | Cos ->
              append_latex " \\cos{\\left(";
              cos (parse_expr ())
          | Tan ->
              append_latex " \\tan{\\left(";
              tan (parse_expr ())
          | Sec ->
              append_latex " \\sec{\\left(";
              1. /. cos (parse_expr ())
          | Csc ->
              append_latex " \\csc{\\left(";
              1. /. sin (parse_expr ())
          | Cot ->
              append_latex " \\cot{\\left(";
              1. /. tan (parse_expr ())
          | Arcsin ->
              append_latex " \\sin^{-1}{\\left(";
              asin (parse_expr ())
          | Arccos ->
              append_latex " \\cos^{-1}{\\left(";
              acos (parse_expr ())
          | Arctan ->
              append_latex " \\tan^{-1}{\\left(";
              atan (parse_expr ())
          | Arcsec ->
              append_latex " \\sec^{-1}{\\left(";
              acos (1. /. parse_expr ())
          | Arccsc ->
              append_latex " \\csc^{-1}{\\left(";
              asin (1. /. parse_expr ())
          | Arccot ->
              append_latex " \\cot^{-1}{\\left(";
              atan (1. /. parse_expr ())
        in
        consume (Parentheses RParen);
        if !abs then begin
          append_latex " \\right|";
          abs := false
        end
        else append_latex " }\\right)";
        f_output
    | Variable X | Variable Y ->
        if is_function_x then begin
          append_latex " x";
          consume (Variable X)
        end
        else begin
          append_latex " y";
          consume (Variable Y)
        end;
        input
    | Constant c -> (
        next ();
        match c with
        | Pi ->
            append_latex " \\pi";
            Float.pi
        | E ->
            append_latex " e";
            2.71828182845904523
        | Number n ->
            append_latex (" " ^ string_of_float n);
            n)
    | _ ->
        Tokenizer.syntax_error
          "expected character not found. [parse elem failure]"
  and parse_expr () =
    let term = ref (parse_term ()) in
    while peek_check (Operator Plus) || peek_check (Operator Minus) do
      if peek_check (Operator Plus) then begin
        next ();
        append_latex " +";
        term := !term +. parse_term ()
      end
      else begin
        next ();
        append_latex " -";
        term := !term -. parse_term ()
      end
    done;
    !term
  and parse_term () =
    let factor = ref (parse_factor ()) in
    while peek_check (Operator Times) || peek_check (Operator Divide) do
      if peek_check (Operator Times) then begin
        next ();
        append_latex " \\cdot";
        factor := !factor *. parse_factor ()
      end
      else begin
        next ();
        append_latex " \\div";
        factor := !factor /. parse_factor ()
      end
    done;
    !factor
  and parse_factor () =
    if peek_check (Operator Minus) then begin
      next ();
      append_latex " -";
      -1. *. parse_factor ()
    end
    else parse_group ()
  and parse_group () =
    let elem = parse_elem () in
    match peek () with
    | Operator Exponent ->
        next ();
        append_latex "^{";
        let elem2 = parse_elem () in
        append_latex " }";
        elem ** elem2
    | Operator _ | Parentheses RParen | EOF -> elem
    | _ -> elem *. parse_group ()
  in

  let parse_equation () =
    if peek_check (if is_function_x then Variable Y else Variable X)
    then begin
      next ();
      consume (Operator Equals);
      if is_function_x then append_latex "y=" else append_latex "x=";
      let expr = parse_expr () in
      consume EOF;
      expr
    end
    else
      let expr = parse_expr () in
      consume (Operator Equals);
      consume (if is_function_x then Variable Y else Variable X);
      consume EOF;
      if is_function_x then append_latex " =y" else append_latex " =x";
      expr
  in

  parse_equation ()
