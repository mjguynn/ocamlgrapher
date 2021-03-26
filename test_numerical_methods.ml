open OUnit2
open Numericalmethods

(* Testing of numerical_methods *)

let range_test name input exp_output range =
  name >:: fun _ -> assert_equal exp_output (range_limiter input range)

let suite : test = []

let _ = run_test_tt_main suite
