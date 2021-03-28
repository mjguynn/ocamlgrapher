open OUnit2
open Tokenizer

let tokenizer_test name input exp_output =
  name >:: fun _ -> assert_equal (tokenize input) exp_output

let suite =
  "Parser Suite"
  >::: [
         tokenizer_test "Two variables" "x+y"
           [ Variable X; Plus; Variable Y ];
         tokenizer_test "Variable times number" "y*23425"
           [ Variable Y; Times; Constant (Number 23425.) ];
         tokenizer_test "Exponential function" "e^x+x^2+4"
           [
             Constant E;
             Exponent;
             Variable X;
             Plus;
             Variable X;
             Exponent;
             Constant (Number 2.);
             Plus;
             Constant (Number 4.);
           ];
         tokenizer_test "Transcendental function"
           "sin(x)+cos(x)-arccot(y^2-ln(4/y))"
           [
             Function Sin;
             LParen;
             Variable X;
             RParen;
             Plus;
             Function Cos;
             LParen;
             Variable X;
             RParen;
             Minus;
             Function Arccot;
             LParen;
             Variable Y;
             Exponent;
             Constant (Number 2.);
             Minus;
             Function Ln;
             LParen;
             Constant (Number 4.);
             Divide;
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
             Exponent;
             Constant (Number 2.);
             RParen;
             Plus;
             Constant (Number 25.22);
             Times;
             Minus;
             Constant (Number 6.);
             Variable Y;
             Minus;
             Constant (Number 3.);
           ];
       ]

let _ = run_test_tt_main suite
