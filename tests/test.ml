open OUnit2

let suite =
  "OCamlGrapher Test Suite"
  >::: List.flatten
         [
           Test_parser.all_tests;
           (* ^ also tests tokenizer *)
           Test_numericalmethods.all_tests;
           Test_config.all_tests;
           (* ^ also tests cmdline *)
           Test_common.all_tests;
         ]

let _ = run_test_tt_main suite
