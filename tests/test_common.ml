open OUnit2
open Common

(** [test_startswith str sub expected] creates an OUnit test case named
    [name] which asserts that [starts_with str sub = expected].*)
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

(** [test_drop str n expected] creates an OUnit test case named [name]
    which asserts that [drop str n = expected].*)
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

(** [test_drop pred lst expected] creates an OUnit test case named
    [name] which asserts that [test_split pred lst = expected].*)
let test_split name pred lst expected =
  name >:: fun _ ->
  assert_equal
    ~printer:(string_of_list (string_of_list (String.make 1)))
    expected (split pred lst)

let simple_pred = ( = ) 'a'

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
  ]

let suite =
  "ocamlgrapher [Common] test suite"
  >::: List.flatten [ starts_with_tests; drop_tests; split_tests ]

let _ = run_test_tt_main suite
