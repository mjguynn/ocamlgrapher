(** [main ()] is the entry point for ocamlgrapher. *)

open Config
open Parser
open Numericalmethods
open Tokenizer

let rec domain_maker_help current_dom_low domain_high step_size acc =
  match current_dom_low < domain_high -. step_size with
  | true ->
      domain_maker_help
        (current_dom_low +. step_size)
        domain_high step_size
        ((current_dom_low +. step_size) :: acc)
  | false -> List.rev acc

(* Outputs list of domain sample points *)
let domain_maker domain steps =
  match domain with
  | d_low, d_high ->
      let step_size = (d_high -. d_low) /. float_of_int steps in
      domain_maker_help d_low d_high step_size [ d_low ]

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
  print_string "Approximate RIGHT-MOST maximum (x, y): \n";
  lst |> get_t |> max_output |> tuple_print;
  print_string "Approximate RIGHT-MOST minimum (x, y): \n";
  lst |> get_t |> min_output |> tuple_print

(* Pretty Print the input-output stuff *)
let rec pp_list_of_lists lst =
  match lst with
  | [] -> print_string "\n"
  | h :: t ->
      tuple_list_print h;
      roots_print h;
      max_and_min_printer h;
      pp_list_of_lists t

(* Graphs the given function given command line inputs. Requires that
   the command line is [Ok]. *)
let main_grapher (config : Config.t) =
  let domain_list = domain_maker (domain config) (steps config) in
  let eqts = equations config in
  let input_output =
    multi_fun_outputs eqts domain_list (range config) []
  in
  pp_list_of_lists input_output

let main () =
  match
    Config.from_cmdline (-10., 10.) (-10., 10.) 100 stdin Sys.argv
  with
  | Error e -> Printf.eprintf "%s\n" e
  | Ok cfg ->
      print_endline (Config.to_string cfg);
      main_grapher cfg

let () = main ()
