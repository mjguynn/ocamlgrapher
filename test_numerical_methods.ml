open OUnit2
open Numericalmethods

(* Testing of numerical_methods *)

let output_1 : t = []

let range_test name (input : (float * float) list) exp_output range =
  name >:: fun _ -> assert_equal exp_output (range_limiter input range)

let suite =
  "Numerical Methods Suite"
  >::: [
         range_test "Basic Example"
           [
             (1.0, 2.3);
             (0.5, 0.4);
             (0.0, -0.5);
             (-0.5, -0.7);
             (-1.0, -1.7);
           ]
           output_1 (0.0, 0.0);
       ]

let _ = run_test_tt_main suite
