(** Entry point for ocamlgrapher. *)

open Config
open Common
open Parser
open Numericalmethods

(** [make_samples (low, high) steps] generates a list of [steps] values
    evenly distributed between [low] and [high].*)
let make_samples (low, high) steps =
  let step_size = span (low, high) /. float_of_int steps in
  let rec do_step cur acc =
    if cur < low then acc else do_step (cur -. step_size) (cur :: acc)
  in
  do_step high []

(** [eval_equation eq samples x_bounds y_bounds] tokenizes [eq] and
    evaluates it at every point in [samples], returning a list of points
    (with all points outside of [x_bounds] and [y_bounds] discarded).
    Requires: [x_bounds] an [y_bounds] are valid bounds.*)
let eval_equation eq samples x_bounds y_bounds =
  let eq_tokenized = Tokenizer.tokenize eq in
  samples
  |> List.map (fun v -> (v, compute_f_of_x eq_tokenized v))
  |> limiter x_bounds y_bounds

(** [print_point_list] prints a list of points to stdout, with each
    point on a new line.*)
let rec print_point_list =
  List.iter (fun (x, y) ->
      Printf.printf "(%10g, %-10g)\n" (trunc x) (trunc y))

(** [print_float_list] prints a list of floats to stdout, with each
    float on a new line.*)
let print_float_list =
  List.iter (fun x -> Printf.printf "%10g\n" (trunc x))

(** [print_stylized s] prints a stylized version of [s] to stdout.*)
let print_stylized s =
  print_string
    (style stdout "96" ^ style stdout "1" ^ s ^ style stdout "0")

(** [print_roots (eq, points)] estimates and prints the roots of [eq]
    given [points], a list of points satisfying [eq].*)
let print_roots (eq, points) =
  print_stylized ("Approximate roots (X-axis) for " ^ eq ^ ": \n");
  points |> root_estimator |> print_float_list

(** [print_points (eq, points)] prints the points [points] satisfying
    [eq].*)
let print_points (eq, points) =
  print_stylized ("Points satisfying " ^ eq ^ ": \n");
  points |> print_point_list

let extrema_printer (eq, points) =
  print_stylized ("Approximate maximums for " ^ eq ^ ": \n");
  points |> max_output |> print_point_list;
  print_stylized ("Approximate minimums " ^ eq ^ ": \n");
  points |> min_output |> print_point_list

(** Executes OCamlgrapher using [config]. *)
let main_grapher (config : Config.t) =
  let x_b, y_b = (x_bounds config, y_bounds config) in
  let x_samples = make_samples x_b (steps config) in
  (* (equation string, list of points satisfying the equation )*)
  let processed =
    equations config
    |> List.map (fun eq -> (eq, eval_equation eq x_samples x_b y_b))
  in
  match command config with
  | Graph ->
      List.fold_left
        (fun g (eq, points) -> Grapher.add_plot eq [ points ] g)
        (Grapher.create (x_bounds config) (y_bounds config))
        processed
      |> Grapher.to_svg (output_file config)
  | Points -> List.iter print_points processed
  | Extrema -> List.iter extrema_printer processed
  | Roots -> List.iter print_roots processed

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
