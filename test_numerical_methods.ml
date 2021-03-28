open OUnit2
open Numericalmethods

(* Testing of numerical_methods *)

let range_test name input exp_output range =
  name >:: fun _ -> assert_equal exp_output (range_limiter input range)

let root_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (root_estimator input)

let suite =
  "Numerical Methods Suite"
  >::: [
         range_test "Return empty list"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, -0.7);
                (1.0, -1.7);
              ])
           (get_t []) (0.0, 0.0);
         range_test "Return Basic 1"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, -0.7);
                (1.0, -1.7);
              ])
           (get_t [ (-0.5, 0.4); (0.0, -0.5); (0.5, -0.7) ])
           (-1.0, 1.0);
         range_test "Return Basic 2: point equal to lower range limit"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, -0.7);
                (1.0, -1.7);
              ])
           (get_t [ (-0.5, 0.4); (0.0, -0.5) ])
           (-0.5, 0.5);
         range_test "Return Basic: point equal to upper range limit"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, -0.7);
                (1.0, -1.7);
              ])
           (get_t [ (-0.5, 0.4); (0.0, -0.5); (0.5, -0.7) ])
           (-1.0, 0.4);
         range_test
           "Return Basic: upper limit = lower limit, point within"
           (get_t
              [
                (1.0, 2.3);
                (0.5, 0.4);
                (0.0, -0.5);
                (-0.5, -0.7);
                (-1.0, -1.7);
              ])
           (get_t [ (0.0, -0.5) ])
           (-0.5, -0.5);
         (*range_test "Should raise exception" (get_t [ (1.0, 2.3);
           (0.5, 0.4); (0.0, -0.5); (-0.5, -0.7); (-1.0, -1.7); ])
           (get_t [ (0.5, 0.4); (0.0, -0.5); (-0.5, -0.7) ]) (1.0,
           -1.0);*)
         root_test "no roots"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, 0.5);
                (0.5, 0.7);
                (1.0, 1.7);
              ])
           [];
         root_test "one root"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, -0.7);
                (1.0, -1.7);
              ])
           [ -0.25 ];
         root_test "two roots"
           (get_t
              [
                (-1.0, 2.3);
                (-0.5, 0.4);
                (0.0, -0.5);
                (0.5, 0.7);
                (1.0, 1.7);
              ])
           [ -0.25; 0.25 ];
       ]

let _ = run_test_tt_main suite
