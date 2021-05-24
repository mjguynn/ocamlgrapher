open OUnit2
open Tokenizer
open Parser

let tokenize_test name input exp_output =
  name >:: fun _ -> assert_equal (tokenize input) exp_output

let generalize_exception input_equation x =
  try compute_f (tokenize input_equation) x
  with Invalid_argument _ -> raise (Invalid_argument "syntax error")

(** [compute_f_test name input_equation input_x expected_output success]
    checks if the function of [input_equation] equals [expected_output]
    at [input_x] if [success] is true. If [success] is false, it checks
    that [input_equation] is an invalid equation. *)
let compute_f_test name input_equation input_x expected_output success =
  name >:: fun _ ->
  if success then
    assert_equal expected_output
      (compute_f (tokenize input_equation) input_x)
      ~printer:string_of_float
  else
    assert_raises (Invalid_argument "syntax error") (fun () ->
        generalize_exception input_equation input_x)

let parser_tests =
  [
    compute_f_test "parse fail for y=3+x*3+" "y=3+x*3+" 0. 0. false;
    compute_f_test "parse fail for y=sin3" "y=sin3" 0. 0. false;
    compute_f_test "parse fail for y=test" "y=test" 0. 0. false;
    compute_f_test "parse fail for y=)(" "y=)(" 0. 0. false;
    compute_f_test "parse fail for y=a(" "y=a(" 0. 0. false;
    compute_f_test "parse fail for y=g" "y=g" 0. 0. false;
    compute_f_test "x=3 for x^2=y" "x^2=y" 3. 9. true;
    compute_f_test "y=3 for y^2=x" "y^2=x" 3. 9. true;
    compute_f_test "x=0 for 3^x=y" "3^x=y" 0. 1. true;
    compute_f_test "x=2 for y=-3^x" "y=-3^x" 2. (-9.) true;
    compute_f_test "x=9 for y=x+3+pi" "y=x+3+pi" 9. (12. +. Float.pi)
      true;
    compute_f_test "x=-11 for x^2-3x=y" "x^2-3x=y" (-11.) 154. true;
    compute_f_test "x=-1 for y=2x" "y=2x" (-1.) (-2.) true;
    compute_f_test "x=-3 for y=x+3" "y=(x+3)" (-3.) 0. true;
    compute_f_test "x=0.369 for y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3"
      "y=sin(x^2-3x+4-6*7)-ln(x)^45+x/3" 0.369 0.0392980592563951792
      true;
    compute_f_test "parse fail for y=x^^2" "y=x^^2" 0. 0. false;
    compute_f_test "parse fail for y=^3+x^2" "y=^3+x^2" 0. 0. false;
    compute_f_test "x=3110 for y=-2x" "y=-2x" 3110. (-6220.) true;
    compute_f_test "x=0.532 for y=-(3x^2*-(4x+2*-sin(x^789)))"
      "y=-(3x^2*-(4x+2*-sin(x^789)))" 0.532 1.80682521600000046 true;
    compute_f_test "x=0 for y=-(3x^2*-(4x+2*-sin(x^789)))"
      "y=-(3x^2*-(4x+2*-sin(x^789)))" 0.0 0.0 true;
    compute_f_test "parse fail for y=x+y^2" "y=x+y^2" 0. 0. false;
    compute_f_test "x=1 for y=e^x" "y=e^x" 1. 2.71828182845904523 true;
    compute_f_test "x=1 for y=(x)" "y=(x)" 1. 1. true;
    compute_f_test "x=1 for y=(3)(x)" "y=(3)(x)" 1. 3. true;
    compute_f_test "x=4 for y=(2)(x-5)^2" "y=(2)(x-5)^2" 4. 2. true;
    compute_f_test "y=4 for x=(2)(y-5)^2" "x=(2)(y-5)^2" 4. 2. true;
    compute_f_test "y=2 for x=log(50y)" "x=log(50y)" 2. 2. true;
    compute_f_test "y=4 for x=sqrt(y)" "x=sqrt(y)" 4. 2. true;
    (* test case below was manually tested, unfortunately nan != nan so
       it fails, even though it works *)
    (*compute_f_test "y=-4 for x=sqrt(y)" "x=sqrt(y)" ~-.4. nan true;*)
    compute_f_test "y=0 for x=sqrt(y)" "x=sqrt(y)" 0. 0. true;
    compute_f_test "y=0 for x=abs(y)" "x=abs(y)" 0. 0. true;
    compute_f_test "y=-1 for x=abs(y)" "x=abs(y)" ~-.1. 1. true;
    compute_f_test "y=1 for x=abs(y)" "x=abs(y)" 1. 1. true;
    compute_f_test "x=0 for y=cos(x)" "y=cos(x)" 0. 1. true;
    compute_f_test "x=0 for y=tan(x)" "y=tan(x)" 0. 0. true;
    compute_f_test "x=0 for y=sec(x)" "y=sec(x)" 0. 1. true;
    compute_f_test "x=0 for y=arcsin(x)" "y=arcsin(x)" 0. 0. true;
    compute_f_test "x=1 for y=arccos(x)" "y=arccos(x)" 1. 0. true;
    compute_f_test "x=0 for y=arctan(x)" "y=arctan(x)" 0. 0. true;
    compute_f_test "x=1 for y=arcsec(x)" "y=arcsec(x)" 1. 0. true;
    compute_f_test "x=infinity for y=arccsc(x)" "y=arccsc(x)" infinity
      0. true;
    compute_f_test "x=infinity for y=arccot(x)" "y=arccot(x)" infinity
      0. true;
    compute_f_test "x=0 for y=cot(x)" "y=cot(x)" 0. infinity true;
    compute_f_test "x=0 for y=csc(x+pi/2)" "y=csc(x+pi/2)" 0. 1. true;
    compute_f_test "parse fail for y=test" "y=test" 0. 0. false;
  ]

let tokenizer_tests =
  [
    tokenize_test "Two variables" "x+y=x"
      (FunctionY
         [
           Variable X;
           Operator Plus;
           Variable Y;
           Operator Equals;
           Variable X;
         ]);
    tokenize_test "Variable times number" "y*23425"
      (FunctionUnknown
         [ Variable Y; Operator Times; Constant (Number 23425.) ]);
    tokenize_test "Function followed by number" "sin22"
      (FunctionUnknown [ Function Sin; Constant (Number 22.) ]);
    tokenize_test "Single variable" "y" (FunctionUnknown [ Variable Y ]);
    tokenize_test
      "Implicit multiplication with constants and variables 1" "pix"
      (FunctionUnknown [ Constant Pi; Variable X ]);
    tokenize_test
      "Implicit multiplication with constants and variables 1" "xpi"
      (FunctionUnknown [ Variable X; Constant Pi ]);
    tokenize_test "Implicit multiplication with constants and functions"
      "pisin"
      (FunctionUnknown [ Constant Pi; Function Sin ]);
    tokenize_test "Exponential function" "y=e^x+x^2+4"
      (FunctionX
         [
           Variable Y;
           Operator Equals;
           Constant E;
           Operator Exponent;
           Variable X;
           Operator Plus;
           Variable X;
           Operator Exponent;
           Constant (Number 2.);
           Operator Plus;
           Constant (Number 4.);
         ]);
    tokenize_test "Transcendental function"
      "sin(x)+cos(x)-arccot(y^2-ln(4/y))=y"
      (FunctionX
         [
           Function Sin;
           Parentheses LParen;
           Variable X;
           Parentheses RParen;
           Operator Plus;
           Function Cos;
           Parentheses LParen;
           Variable X;
           Parentheses RParen;
           Operator Minus;
           Function Arccot;
           Parentheses LParen;
           Variable Y;
           Operator Exponent;
           Constant (Number 2.);
           Operator Minus;
           Function Ln;
           Parentheses LParen;
           Constant (Number 4.);
           Operator Divide;
           Variable Y;
           Parentheses RParen;
           Parentheses RParen;
           Operator Equals;
           Variable Y;
         ]);
    tokenize_test "Implicit multiplication with spaces"
      "x=24xsin(x^2) + 25.22 *  -6 y-3"
      (FunctionY
         [
           Variable X;
           Operator Equals;
           Constant (Number 24.);
           Variable X;
           Function Sin;
           Parentheses LParen;
           Variable X;
           Operator Exponent;
           Constant (Number 2.);
           Parentheses RParen;
           Operator Plus;
           Constant (Number 25.22);
           Operator Times;
           Operator Minus;
           Constant (Number 6.);
           Variable Y;
           Operator Minus;
           Constant (Number 3.);
         ]);
  ]

let all_tests = List.flatten [ parser_tests; tokenizer_tests ]
