(** Entry point for ocamlgrapher. *)

open Config
open Common
open Parser
open Numericalmethods
open Tokenizer

(** [make_samples (low, high) steps] generates a list of [steps] values
    evenly distributed between [low] and [high].*)
let make_samples (low, high) steps =
  let step_size = span (low, high) /. float_of_int steps in
  let rec do_step cur acc =
    if cur < low then acc else do_step (cur -. step_size) (cur :: acc)
  in
  do_step high []

(* Truncator *)
let trunc x = if abs_float x < 1e-13 then 0. else x

(* get outputs from equation *)
let rec fun_output (eqt : token list) domain_list range acc =
  match domain_list with
  | [] -> acc |> List.rev |> get_t |> fun x -> range_limiter x range
  | h :: t ->
      fun_output eqt t range
        ((h, h |> compute_f_of_x eqt |> trunc) :: acc)

(* Go through list of equations *)
let rec multi_fun_outputs eqts domain_list range acc =
  match eqts with
  | [] -> List.rev acc
  | h :: t ->
      multi_fun_outputs t domain_list range
        (fun_output (tokenize h) domain_list range [] :: acc)

let tuple_print (x, y) =
  Printf.printf "(%10g, %-10g)\n" (trunc x) (trunc y)

let rec tuple_list_print lst =
  match lst with
  | [] -> print_string "\n"
  | (x, y) :: t ->
      tuple_print (x, y);
      tuple_list_print t

let rec float_list_print lst =
  match lst with
  | [] -> print_string "\n"
  | h :: t ->
      Printf.printf "%10g" (trunc h);
      float_list_print t

let roots_print lst =
  print_string "Approximate roots (x-coords): \n";
  lst |> get_t |> root_estimator |> float_list_print

let max_and_min_printer lst =
  print_string "Approximate maximum(s) (x, y): \n";
  lst |> get_t |> max_output |> tuple_list_print;
  print_string "Approximate minimum(s) (x, y): \n";
  lst |> get_t |> min_output |> tuple_list_print

(* Pretty Print the input-output stuff *)
let rec pp_list_of_lists lst =
  match lst with
  | [] -> print_string "\n"
  | h :: t ->
      tuple_list_print h;
      roots_print h;
      max_and_min_printer h;
      pp_list_of_lists t

(** Executes OCamlgrapher using [config]. *)
let main_grapher (config : Config.t) =
  let x_samples = make_samples (x_bounds config) (steps config) in
  let eqts = equations config in
  let input_output =
    multi_fun_outputs eqts x_samples (y_bounds config) []
  in
  let g =
    List.fold_left
      (fun g eq -> Grapher.add_plot eq [] g)
      (Grapher.create (x_bounds config) (y_bounds config))
      eqts
  in
  input_output
  |> List.iter (fun lst ->
         match command config with
         | Graph -> Grapher.to_svg (output_file config) g
         | Points -> tuple_list_print lst
         | Extrema -> max_and_min_printer lst
         | Roots -> roots_print lst)

(** [main ()] is the entry point for ocamlgrapher. *)
let main () =
  match
    from_cmdline (-10., 10.) (-10., 10.) 100 "out.svg" stdin Sys.argv
  with
  | Error e -> Printf.eprintf "%s\n" e
  | Ok cfg ->
      print_endline (to_string cfg);
      main_grapher cfg

let () = main ()
