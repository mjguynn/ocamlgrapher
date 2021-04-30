(** Entry point for ocamlgrapher. *)

open Config
open Common
open Numericalmethods

(** [make_samples (low, high) steps] generates a list of [steps] values
    evenly distributed between [low] and [high].*)
let make_samples (low, high) steps =
  let step_size = span (low, high) /. float_of_int steps in
  let rec do_step cur acc =
    if cur <= low then acc else do_step (cur -. step_size) (cur :: acc)
  in
  do_step high []

(** [eval_equation eq samples] tokenizes [eq] and evaluates it at every
    value in [samples], returning a list of list of points. Each
    "sub-list" (list of points) represents a continous segment of [eq].
    For example, if [eq] is "y=4x^2", then each value in samples is
    interpreted as a value of [x] and plugged in to the equation; if it
    were "x=4y^2", each value in samples is interpreted as a value of
    [y]. *)
let eval_equation eq samples =
  try
    let eq_tokenized = Tokenizer.tokenize eq in
    samples
    |> List.map (fun v -> (v, [ Parser.compute_f_of_x eq_tokenized v ]))
  with Invalid_argument s ->
    Io.print_error (s ^ "\n");
    []

(** [print_point_list] prints a list of points to stdout, with each
    point on a new line.*)
let rec print_point_list =
  List.iter (fun (x, y) ->
      Printf.printf "(%10g, %-10g)\n" (trunc x) (trunc y))

(** [print_float_list] prints a list of floats to stdout, with each
    float on a new line.*)
let print_float_list =
  List.iter (fun x -> Printf.printf "%10g\n" (trunc x))

(** [print_roots (eq, points)] estimates and prints the roots of [eq]
    given [points], a list of points satisfying [eq].*)
let print_roots (eq, points) =
  Io.print_header ("Approximate roots (X-axis) for " ^ eq ^ ": \n");
  List.flatten points |> root_estimator |> print_float_list

(** [print_points (eq, points)] prints the points [points] satisfying
    [eq].*)
let print_points (eq, points) =
  Io.print_header ("Points satisfying " ^ eq ^ ": \n");
  List.flatten points |> print_point_list

let extrema_printer (eq, points) =
  Io.print_header ("Approximate maximums for " ^ eq ^ ": \n");
  List.flatten points |> max_output |> print_point_list;
  Io.print_header ("Approximate minimums " ^ eq ^ ": \n");
  List.flatten points |> min_output |> print_point_list

(** Executes OCamlgrapher using [config]. *)
let main_grapher (config : Config.t) =
  let x_b, y_b = (x_bounds config, y_bounds config) in
  let x_samples = make_samples x_b (steps config) in
  (* (equation string, list of points satisfying the equation )*)
  let eqs_pre_limiter =
    equations config
    |> List.map (fun eq -> (eq, eval_equation eq x_samples))
  in
  let eqs =
    eqs_pre_limiter
    |> List.map (fun (eq, ps) ->
           (eq, ps |> List.map (limiter x_b y_b) |> List.flatten))
  in
  match command config with
  | Graph ->
      List.fold_left
        (fun g (eq, points) -> Grapher.add_plot eq points g)
        (Grapher.create (x_bounds config) (y_bounds config))
        [ eqs_pre_limiter ]
      |> Grapher.to_svg (output_file config);
      (* print the output file to stdout so the user can pipe it *)
      print_endline (output_file config)
  | Points -> List.iter print_points eqs
  | Extrema -> List.iter extrema_printer eqs
  | Roots -> List.iter print_roots eqs

(** [main ()] is the entry point for ocamlgrapher. *)
let main () =
  match
    from_cmdline (-10., 10.) (-10., 10.) 500 "out.svg" stdin Sys.argv
  with
  | Error e -> Io.print_error (e ^ "\n")
  | Ok cfg ->
      (* Write the command to stderr since we don't want this showing up
         if the user is outputting to a file. *)
      Io.print_header ~channel:stderr "Interpreted Command: ";
      Io.print_detail ~channel:stderr (to_string cfg);
      prerr_newline ();
      (* don't concat a "\n" onto the above string -- prerr_newline
         flushes output, which is required for correct ordering here*)
      main_grapher cfg

let () = main ()
