open OUnit2
open Tokenizer

let tokenizer_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (tokenize input)

let suite : test =
  [
    tokenizer_test "test1" "x+y+43"
      [ Variable X; Plus; Variable Y; Constant (Number 43.) ];
  ]

let _ = run_test_tt_main suite
