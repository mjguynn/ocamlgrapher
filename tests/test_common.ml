open OUnit2
open Common

(** [test_startswith name str sub expected] creates an OUnit test case
    named [name] which asserts that [starts_with str sub = expected].*)
let test_starts_with name str sub expected =
  name >:: fun _ -> assert_equal expected (starts_with str sub)

let starts_with_tests =
  [
    test_starts_with "empty string starts with empty string" "" "" true;
    test_starts_with "non-empty string starts with empty string" "hey"
      "" true;
    test_starts_with "empty string doesn't start with nonempty string"
      "" "hey" false;
    test_starts_with "string starts with its first few characters" "hey"
      "he" true;
    test_starts_with "string doesn't start with some characters" "hey"
      "ey" false;
    test_starts_with "string starts with itself" "hey" "hey" true;
    test_starts_with
      "string doesn't start with longer version of itself" "hey" "heyo"
      false;
  ]

(** [test_drop name str n expected] creates an OUnit test case named
    [name] which asserts that [drop str n = expected].*)
let test_drop name str n expected =
  name >:: fun _ -> assert_equal ~printer:Fun.id expected (drop str n)

let drop_tests =
  [
    test_drop "empty string drop 0 is empty string" "" 0 "";
    test_drop "empty string drop >0 is empty string" "" 1 "";
    test_drop "string drop 0 is itself" "what" 0 "what";
    test_drop "string drop 1" "what" 1 "hat";
    test_drop "string drop 2" "what" 2 "at";
    test_drop "string drop = its length is empty" "what" 4 "";
    test_drop "string drop > its length is empty" "what" 5 "";
  ]

let string_of_list string_of_element_fun l =
  Printf.sprintf "[ %s]"
    (List.fold_left
       (fun acc e -> acc ^ string_of_element_fun e ^ "; ")
       "" l)

(** [test_split name pred lst expected] creates an OUnit test case named
    [name] which asserts that [split pred lst = expected].*)
let test_split name pred lst expected =
  name >:: fun _ ->
  assert_equal
    ~printer:(string_of_list (string_of_list (String.make 1)))
    expected (split pred lst)

let simple_pred _ c _ = c = 'a'

(* like it or not, this is peak predicate *)
let peak_pred p c n =
  c >= Option.value p ~default:'z' && c >= Option.value n ~default:'z'

let split_tests =
  [
    test_split "empty" simple_pred [] [];
    test_split "no splitters" simple_pred [ 'b'; 'c'; 'd' ]
      [ [ 'b'; 'c'; 'd' ] ];
    test_split "one splitter" simple_pred
      [ 'b'; 'c'; 'a'; 'd'; 'u' ]
      [ [ 'b'; 'c' ]; [ 'd'; 'u' ] ];
    test_split "two splitters" simple_pred
      [ 'b'; 'c'; 'a'; 'd'; 'u'; 'a'; 'a'; 'c'; 'v' ]
      [ [ 'b'; 'c' ]; [ 'd'; 'u' ]; [ 'c'; 'v' ] ];
    test_split "oops! all splitters" simple_pred [ 'a'; 'a' ] [];
    test_split "end with splitter" simple_pred
      [ 'c'; 'a'; 'd'; 'g'; 'a' ]
      [ [ 'c' ]; [ 'd'; 'g' ] ];
    test_split "complex pred, in the middle" peak_pred
      [ 'a'; 'b'; 'c'; 'b'; 'a' ]
      [ [ 'a'; 'b' ]; [ 'b'; 'a' ] ];
    test_split "complex pred, flatline & stuff " peak_pred
      [ 'a'; 'b'; 'c'; 'c'; 'c'; 'c'; 'b'; 'c'; 'd'; 'e'; 'a' ]
      [ [ 'a'; 'b' ]; [ 'b'; 'c'; 'd' ]; [ 'a' ] ];
  ]

(** [test_regular_float name flt expected] creates an OUnit test case
    named [name] which asserts that [regular_float flt = expected].*)
let test_regular_float name flt expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected (regular_float flt)

let float_tests =
  [
    test_regular_float "positive zero is regular" 0.0 true;
    test_regular_float "negative zero is regular" (-0.0) true;
    test_regular_float "positive regular number" 12.7 true;
    test_regular_float "negative regular number" (-87.2) true;
    test_regular_float "positive inf irregular" infinity false;
    test_regular_float "negative inf irregular" ~-.infinity false;
    test_regular_float "NaN irregular" nan true;
  ]

(* fpeq is tested by test_numericalmethods & was tested manually, in
   real circumstances, much better than I could do here. *)

(* I totally just eyeballed trunc to make the output look pretty so
   these tests are pretty vague*)

let test_trunc_base name flt expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected (trunc flt)

let test_trunc_ident name flt = test_trunc_base name flt flt

let test_trunc_zero name flt = test_trunc_base name flt 0.0

let small_flt = 0.0000000001

let tiny_flt = 1e-16

let trunc_tests =
  [
    test_trunc_ident "trunc normal number" 5.6;
    test_trunc_ident "trunc larger normal number" 1056.7;
    test_trunc_ident "trunc +inf" infinity;
    test_trunc_ident "trunc -inf" ~-.infinity;
    test_trunc_ident "trunc negative number" ~-.12.0;
    test_trunc_ident "trunc zero" 0.0;
    test_trunc_ident "trunc small float" small_flt;
    test_trunc_ident "trunc negative small float" ~-.small_flt;
    test_trunc_zero "trunc TINY float" tiny_flt;
    test_trunc_zero "trunc negative tiny float" ~-.tiny_flt;
  ]

let test_oob name x_b y_b input expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected
    (point_oob x_b y_b input)

let oob_tests =
  [
    test_oob "in bounds" (-5.0, 5.0) (-5.0, 5.0)
      (Some (0.0, 0.05))
      false;
    test_oob "out bounds y" (-5.0, 5.0) (-5.0, 5.0)
      (Some (0.0, 10.0))
      true;
    test_oob "out bounds x" (-5.0, 5.0) (-5.0, 5.0)
      (Some (-6.25, 4.0))
      true;
    test_oob "out bounds x AND y" (-5.0, 5.0) (-5.0, 5.0)
      (Some (-6.25, 12.0))
      true;
    test_oob "nonexistent point" (-5.0, 5.0) (-5.0, 5.0) None true;
  ]

let suite =
  "ocamlgrapher [Common] test suite"
  >::: List.flatten
         [
           starts_with_tests;
           drop_tests;
           split_tests;
           trunc_tests;
           oob_tests;
         ]

let _ = run_test_tt_main suite
