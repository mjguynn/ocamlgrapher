open OUnit2
open Tokenizer
open Parser

let tokenizer_test name input exp_output =
  name >:: fun _ -> assert_equal (tokenize input) exp_output

let generalize_exception input =
  try parse input
  with Invalid_argument _ -> raise (Invalid_argument "syntax error")

let parser_test name input success =
  name >:: fun _ ->
  if success then assert_equal () (parse input)
  else
    assert_raises (Invalid_argument "syntax error") (fun () ->
        generalize_exception input)

let parser_tests =
  [
    parser_test "parse y=3+x*3+" "y=3+x*3+" false;
    parser_test "parse x^2=y" "x^2=y" true;
    parser_test "parse y=x+3+pi" "y=x+3+pi" true;
    parser_test "parse x^2-3x=y" "x^2-3x=y" true;
    parser_test "parse y=2x" "y=2x" true;
    parser_test "parse y=x+3" "y=(x+3)" true;
    parser_test "parse y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3"
      "y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3" true;
    parser_test "parse y=x^^2" "y=x^^2" false;
    parser_test "parse y=^3+x^2" "y=^3+x^2" false;
    parser_test "y=-3x" "y=-3x" true;
    parser_test "y=-(3x^2*-(4x+2*-sin(x^789)))"
      "y=-(3x^2*-(4x+2*-sin(x^789)))" true;
  ]

let tokenizer_tests =
  [
    tokenizer_test "Two variables" "x+y"
      [ Variable X; Operator Plus; Variable Y ];
    tokenizer_test "Variable times number" "y*23425"
      [ Variable Y; Operator Times; Constant (Number 23425.) ];
    tokenizer_test "Exponential function" "e^x+x^2+4"
      [
        Constant E;
        Operator Exponent;
        Variable X;
        Operator Plus;
        Variable X;
        Operator Exponent;
        Constant (Number 2.);
        Operator Plus;
        Constant (Number 4.);
      ];
    tokenizer_test "Transcendental function"
      "sin(x)+cos(x)-arccot(y^2-ln(4/y))"
      [
        Function Sin;
        Operator LParen;
        Variable X;
        Operator RParen;
        Operator Plus;
        Function Cos;
        Operator LParen;
        Variable X;
        Operator RParen;
        Operator Minus;
        Function Arccot;
        Operator LParen;
        Variable Y;
        Operator Exponent;
        Constant (Number 2.);
        Operator Minus;
        Function Ln;
        Operator LParen;
        Constant (Number 4.);
        Operator Divide;
        Variable Y;
        Operator RParen;
        Operator RParen;
      ];
    tokenizer_test "Implicit multiplication with spaces"
      "24xsin(x^2) + 25.22 *  -6 y-3"
      [
        Constant (Number 24.);
        Variable X;
        Function Sin;
        Operator LParen;
        Variable X;
        Operator Exponent;
        Constant (Number 2.);
        Operator RParen;
        Operator Plus;
        Constant (Number 25.22);
        Operator Times;
        Operator Minus;
        Constant (Number 6.);
        Variable Y;
        Operator Minus;
        Constant (Number 3.);
      ];
  ]

let suite =
  "Parser Test Suite"
  >::: List.flatten [ parser_tests; tokenizer_tests ]

let _ = run_test_tt_main suite
