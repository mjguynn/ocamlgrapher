open OUnit2
open Tokenizer
open Parser

let tokenizer_test name input exp_output =
  name >:: fun _ -> assert_equal (tokenize input) exp_output

let parser_test name input =
  name >:: fun _ -> assert_equal () (parse input)

let suite =
  "Parser Suite"
  >::: [
         parser_test "parser test" "x=3pi^2+2";
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
             LParen;
             Variable X;
             RParen;
             Operator Plus;
             Function Cos;
             LParen;
             Variable X;
             RParen;
             Operator Minus;
             Function Arccot;
             LParen;
             Variable Y;
             Operator Exponent;
             Constant (Number 2.);
             Operator Minus;
             Function Ln;
             LParen;
             Constant (Number 4.);
             Operator Divide;
             Variable Y;
             RParen;
             RParen;
           ];
         tokenizer_test "Implicit multiplication with spaces"
           "24xsin(x^2) + 25.22 *  -6 y-3"
           [
             Constant (Number 24.);
             Variable X;
             Function Sin;
             LParen;
             Variable X;
             Operator Exponent;
             Constant (Number 2.);
             RParen;
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

let _ = run_test_tt_main suite
