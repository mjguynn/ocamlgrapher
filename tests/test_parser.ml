open OUnit2
open Tokenizer
open Parser

let tokenize_test name input exp_output =
  name >:: fun _ -> assert_equal (tokenize input) exp_output

let generalize_exception tokens x =
  try compute_f_of_x tokens x
  with Invalid_argument _ -> raise (Invalid_argument "syntax error")

(** [compute_f_of_x_test name input_equation input_x expected_output success]
    checks if the function of [input_equation] equals [expected_output]
    at [input_x] if [success] is true. If [success] is false, it checks
    that [input_equation] is an invalid equation. *)
let compute_f_of_x_test
    name
    input_equation
    input_x
    expected_output
    success =
  name >:: fun _ ->
  if success then
    assert_equal expected_output
      (compute_f_of_x (tokenize input_equation) input_x)
  else
    assert_raises (Invalid_argument "syntax error") (fun () ->
        generalize_exception (tokenize input_equation) input_x)

let parser_tests =
  [
    compute_f_of_x_test "parse fail for y=3+x*3+" "y=3+x*3+" 0. 0. false;
    compute_f_of_x_test "x=3 for x^2=y" "x^2=y" 3. 9. true;
    compute_f_of_x_test "x=0 for 3^x=y" "3^x=y" 0. 1. true;
    compute_f_of_x_test "x=2 for y=-3^x" "y=-3^x" 2. (-9.) true;
    compute_f_of_x_test "x=9 for y=x+3+pi" "y=x+3+pi" 9.
      (12. +. Float.pi) true;
    compute_f_of_x_test "x=-11 for x^2-3x=y" "x^2-3x=y" (-11.) 154. true;
    compute_f_of_x_test "x=-1 for y=2x" "y=2x" (-1.) (-2.) true;
    compute_f_of_x_test "x=-3 for y=x+3" "y=(x+3)" (-3.) 0. true;
    compute_f_of_x_test "x=0.369 for y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3"
      "y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3" 0.369 0.0392980592563951792
      true;
    compute_f_of_x_test "parse fail for y=x^^2" "y=x^^2" 0. 0. false;
    compute_f_of_x_test "parse fail for y=^3+x^2" "y=^3+x^2" 0. 0. false;
    compute_f_of_x_test "x=3110 for y=-2x" "y=-2x" 3110. (-6220.) true;
    compute_f_of_x_test "x=0.532 for y=-(3x^2*-(4x+2*-sin(x^789)))"
      "y=-(3x^2*-(4x+2*-sin(x^789)))" 0.532 1.80682521600000046 true;
    compute_f_of_x_test "x=0 for y=-(3x^2*-(4x+2*-sin(x^789)))"
      "y=-(3x^2*-(4x+2*-sin(x^789)))" 0.0 0.0 true;
    compute_f_of_x_test "parse fail for y=x+y^2" "y=x+y^2" 0. 0. false;
    compute_f_of_x_test "x=1 for y=e^x" "y=e^x" 1. 2.71828182845904523
      true;
  ]

let tokenizer_tests =
  [
    tokenize_test "Two variables" "x+y"
      [ Variable X; Operator Plus; Variable Y ];
    tokenize_test "Variable times number" "y*23425"
      [ Variable Y; Operator Times; Constant (Number 23425.) ];
    tokenize_test "Exponential function" "e^x+x^2+4"
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
    tokenize_test "Transcendental function"
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
    tokenize_test "Implicit multiplication with spaces"
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
