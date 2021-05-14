open OUnit2
open Numericalmethods

(* Testing of numerical_methods *)

let limiter_test name x_bounds y_bounds input exp_output =
  name >:: fun _ ->
  assert_equal exp_output (limiter x_bounds y_bounds input)

let root_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (root_estimator input)

let max_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (max_output input)

let min_test name input exp_output =
  name >:: fun _ -> assert_equal exp_output (min_output input)

let close_enough =
  List.fold_left2 (fun acc a b -> acc && Common.fpeq a b) true

let samples_test name bounds steps exp_output =
  name >:: fun _ ->
  assert_equal ~cmp:close_enough exp_output (make_samples bounds steps)

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
         root_test "no roots"
           [
             (-1.0, 2.3); (-0.5, 0.4); (0.0, 0.5); (0.5, 0.7); (1.0, 1.7);
           ]
           [];
         root_test "empty input" [] [];
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
         samples_test "one sample" (0.0, 1.0) 1 [ 1.0 ];
         samples_test "two sample" (0.0, 1.0) 2 [ 0.5; 1.0 ];
         samples_test "three sample" (0.0, 1.0) 3
           [ 1.0 /. 3.0; 2.0 /. 3.0; 1.0 ];
         samples_test "negative & positive sample" (-1.0, 1.0) 4
           [ -0.5; 0.0; 0.5; 1.0 ];
         (* exception testing *)
         ( "limiter invalid x bounds" >:: fun _ ->
           assert_raises Invalid_bounds (fun () ->
               limiter (8.0, 7.0) (6.0, 7.0) []) );
         ( "limiter invalid y bounds" >:: fun _ ->
           assert_raises Invalid_bounds (fun () ->
               limiter (-7.0, 7.0) (~-.infinity, 7.0) []) );
         ( "min_output no points" >:: fun _ ->
           assert_raises No_points (fun () -> max_output []) );
         ( "max_output no points" >:: fun _ ->
           assert_raises No_points (fun () -> min_output []) );
       ]

let _ = run_test_tt_main suite
