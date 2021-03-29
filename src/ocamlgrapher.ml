(** [main ()] is the entry point for ocamlgrapher. *)

open Config
open Parser

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

(* get outputs from equation *)
let rec fun_output eqt domain_list acc =
  match domain_list with
  | [] -> List.rev acc
  | h :: t -> fun_output eqt t ((h, compute_f_of_x eqt h) :: acc)

(* Go through list of equations *)
let rec multi_fun_outputs eqt domain_list acc =
  match eqt with
  | [] -> List.rev acc
  | h :: t ->
      multi_fun_outputs t domain_list
        (fun_output h domain_list [] :: acc)

let tuple_print (x, y) =
  print_endline
    ("[" ^ Printf.sprintf "%.5f" x ^ ", " ^ Printf.sprintf "%.5f" y
   ^ "]")

let rec tuple_list_print lst =
  match lst with
  | [] -> print_newline ()
  | h :: t -> tuple_print h ^ tuple_list_print t

(* Pretty Print the input-output stuff *)
let rec pp_list_of_lists lst =
  match lst with
  | [] -> print_newline ()
  | h :: t -> tuple_list_print h ^ pp_list_of_lists t

(* Graphs the given function given command line inputs. Requires that
   the command line is [Ok]. *)
let main_grapher (config : t) =
  let domain_list = domain_maker (domain config) (steps config) in
  let eqts = equations config in
  let input_output = multi_fun_outputs eqts domain_list [] in
  pp_list_of_lists input_output

let main () =
  match
    Config.from_cmdline (-10., 10.) (-10., 10.) 100 stdin Sys.argv
  with
  | Error e -> Printf.eprintf "%s\n" e
  | Ok cfg ->
      print_endline (Config.to_string cfg);
      let temp = main_grapher cfg in
      ()

let () = main ()