open OUnit2
open Numericalmethods

(* Testing of numerical_methods *)

let limiter_test name x_bounds y_bounds input exp_output =
  name >:: fun _ ->
  assert_equal exp_output (limiter x_bounds y_bounds input)

let limiter_2_test name x_bounds y_bounds input exp_output =
  name >:: fun _ ->
  assert_equal exp_output (limiter_2 x_bounds y_bounds input)

let root_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (root_estimator input)

let max_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (max_output input)

let min_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (min_output input)

let suite =
  "Numerical Methods Suite"
  >::: [
         limiter_test "Return empty list" (-10.0, 10.0) (0.0, 0.0)
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, -0.7);
             (1.0, -1.7);
           ]
           [];
         limiter_test "Return Basic 1" (-10.0, 10.0) (-1.0, 1.0)
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, -0.7);
             (1.0, -1.7);
           ]
           [ (-0.5, 0.4); (0.0, -0.5); (0.5, -0.7) ];
         limiter_test "Return Basic 2: point equal to lower range limit"
           (-10.0, 10.0) (-0.5, 0.5)
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, -0.7);
             (1.0, -1.7);
           ]
           [ (-0.5, 0.4); (0.0, -0.5) ];
         limiter_test "Return Basic: point equal to upper range limit"
           (-10.0, 10.0) (-1.0, 0.4)
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, -0.7);
             (1.0, -1.7);
           ]
           [ (-0.5, 0.4); (0.0, -0.5); (0.5, -0.7) ];
         limiter_test
           "Return Basic: upper limit = lower limit, point within"
           (-10.0, 10.0) (-0.5, -0.5)
           [
             (1.0, 2.3);
             (0.5, 0.4);
             (0.0, -0.5);
             (-0.5, -0.7);
             (-1.0, -1.7);
           ]
           [ (0.0, -0.5) ];
         (*limiter_test "Should raise exception" (-10.0, 10.0) (1.0,
           -1.0) [ (1.0, 2.3); (0.5, 0.4); (0.0, -0.5); (-0.5, -0.7);
           (-1.0, -1.7); ] [ (0.5, 0.4); (0.0, -0.5); (-0.5, -0.7) ];*)
         root_test "no roots"
           [
             (-1.0, 2.3); (-0.5, 0.4); (0.0, 0.5); (0.5, 0.7); (1.0, 1.7);
           ]
           [];
         root_test "one root"
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, -0.7);
             (1.0, -1.7);
           ]
           [ -0.25 ];
         root_test "two roots"
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, 0.7);
             (1.0, 1.7);
           ]
           [ -0.25; 0.25 ];
         (*max_test "Failing Test: empty list" (get_t []) (2., 3.);*)
         max_test "Maximum"
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, 0.7);
             (1.0, 1.7);
           ]
           [ (-1.0, 2.3) ];
         max_test "Maximum: list"
           [
             (-1.0, 2.3); (-0.5, 2.3); (0.0, 2.3); (0.5, 2.3); (1.0, 2.3);
           ]
           [
             (-1.0, 2.3); (-0.5, 2.3); (0.0, 2.3); (0.5, 2.3); (1.0, 2.3);
           ];
         min_test "Minimum"
           [
             (-1.0, 2.3);
             (-0.5, 0.4);
             (0.0, -0.5);
             (0.5, 0.7);
             (1.0, 1.7);
           ]
           [ (0.0, -0.5) ];
         min_test "Minimum: list"
           [
             (-1.0, 2.3); (-0.5, 2.3); (0.0, 2.3); (0.5, 2.3); (1.0, 2.3);
           ]
           [
             (-1.0, 2.3); (-0.5, 2.3); (0.0, 2.3); (0.5, 2.3); (1.0, 2.3);
           ];
         (* Testing the second limiter *)
         limiter_2_test "Example in mli file" (-10., 10.) (-10., 10.)
           [ (5., 9.5); (5.5, 9.7); (6., 10.3); (6.5, 9.7); (7., 9.5) ]
           [ [ (5., 9.5); (5.5, 9.7) ]; [ (6.5, 9.7); (7., 9.5) ] ];
         limiter_2_test "Another test" (-10., 10.) (-10., 10.)
           [
             (5., -9.5);
             (5.5, -9.7);
             (6., -10.3);
             (6.5, -9.7);
             (7., -9.5);
           ]
           [ [ (5., -9.5); (5.5, -9.7) ]; [ (6.5, -9.7); (7., -9.5) ] ];
         limiter_2_test "Another test, split to three lists" (-10., 10.)
           (-10., 10.)
           [
             (5., -9.5);
             (5.5, -9.7);
             (6., -10.3);
             (6.5, -9.7);
             (7., -9.5);
             (7.5, -9.7);
             (8., -10.3);
             (8.5, -9.7);
             (9., -9.5);
           ]
           [
             [ (5., -9.5); (5.5, -9.7) ];
             [ (6.5, -9.7); (7., -9.5); (7.5, -9.7) ];
             [ (8.5, -9.7); (9., -9.5) ];
           ];
         limiter_2_test "Another test, split to three lists: ex 2"
           (-10., 10.) (-10., 10.)
           [
             (5., 9.5);
             (5.5, 9.7);
             (6., 10.3);
             (6.5, 9.7);
             (7., 9.5);
             (7.5, 9.7);
             (8., 10.3);
             (8.5, 9.7);
             (9., 9.5);
           ]
           [
             [ (5., 9.5); (5.5, 9.7) ];
             [ (6.5, 9.7); (7., 9.5); (7.5, 9.7) ];
             [ (8.5, 9.7); (9., 9.5) ];
           ];
       ]

let _ = run_test_tt_main suite
