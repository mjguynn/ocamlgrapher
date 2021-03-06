(** Contains the entry point for ocamlgrapher and high-level
    functionality relating to the operation of the program. *)

open Config
open Common
open Numericalmethods

(** An [equation] contains information about a single equation whose
    formula is specified by [text] using the rules defined in
    [Tokenizer] and [Parser]. [graph_data] is a list of point lists,
    where each point list represents a single continuous segment of the
    function evaluated over an arbitrary domain or range, where
    connecting one point to the next point in the list with a line would
    form a reasonable approximation to the function. [query_data] is a
    list of points satisfying [graph_data] on the same (arbitary)
    domain/range but with all points outside of the given bounds
    discarded.*)
type equation = {
  text : string;
  graph_data : points list;
  query_data : points;
}

(** [should_discard x_b y_b prev (x, y) next] returns whether a point
    should be discarded from the list of graphed points. It returns true
    if [x] or [y] is inf/nan OR if [x] is outside of [x_b] or [y] is
    outside of [y_b] and [prev] and [next] are outside of [x_b] X [y_b].*)
let should_discard x_b y_b prev (x, y) next =
  let oob = point_oob x_b y_b in
  (not (regular_float x && regular_float y))
  || (oob prev && oob (Some (x, y)) && oob next)

(** [process_equation text samples x_bounds y_bounds] produces the
    [equation] object specified by [text] and evaluated with [steps]
    samples over [x_bounds] and [y_bounds]. Requires: [x_bounds] an
    [y_bounds] are valid bounds.*)
let process_equation text steps x_b y_b =
  try
    let domain, make_points =
      match Tokenizer.tokenize text with
      | FunctionX _ as tl -> (x_b, fun x -> (x, Parser.compute_f tl x))
      | FunctionY _ as tl -> (y_b, fun y -> (Parser.compute_f tl y, y))
      | FunctionUnknown _ ->
          Tokenizer.syntax_error
            "Function of unknown variable. Please note that implicit \
             functions are not supported."
    in
    let graph_data =
      make_samples domain steps
      |> List.map make_points
      |> split (should_discard x_b y_b)
    in
    let query_data = graph_data |> List.flatten |> limiter x_b y_b in
    { text; graph_data; query_data }
  with Invalid_argument s ->
    Io.print_error (text ^ ": " ^ s ^ "\n");
    { text; graph_data = []; query_data = [] }

(** [print_point_list] prints a list of points to stdout, with each
    point on a new line.*)
let rec print_point_list =
  List.iter (fun (x, y) ->
      Printf.printf "(%10g, %-10g)\n" (trunc x) (trunc y))

(** [print_float_list] prints a list of floats to stdout, with each
    float on a new line.*)
let print_float_list =
  List.iter (fun x -> Printf.printf "%10g\n" (trunc x))

(** [print_roots eq] estimates and prints the approximated roots of [eq]
    on its X and Y bounds. *)
let print_roots eq =
  Io.print_header ("Approximate roots (X-axis) for " ^ eq.text ^ ": \n");
  eq.query_data |> root_estimator |> print_float_list

(** [print_points eq] prints the points computed to satisfy [eq] on its
    X and Y bounds. *)
let print_points eq =
  Io.print_header ("Points satisfying " ^ eq.text ^ ": \n");
  print_point_list eq.query_data

(** [print_extrema eq] prints the approximated extrema of [eq] on its X
    and Y bounds. *)
let print_extrema eq =
  Io.print_header ("Approximate maximums for " ^ eq.text ^ ": \n");
  eq.query_data |> max_output |> print_point_list;
  Io.print_header ("Approximate minimums for " ^ eq.text ^ ": \n");
  eq.query_data |> min_output |> print_point_list

(** [make_graph config eqs] creates an SVG graph of the equations in
    [eqs] using the program configuration [config] and prints the
    filename of the resulting graph to [stdout].*)
let make_graph config eqs =
  List.fold_left
    (fun graph eq -> Grapher.add_plot eq.text eq.graph_data graph)
    (Grapher.create (x_bounds config) (y_bounds config) (ratio config))
    eqs
  |> Grapher.to_svg (output_file config);
  (* flush stderr stream, required for correct ordering of the print
     below if the user input an equation with an error *)
  flush stderr;
  (* print the output file to stdout so the user can pipe it *)
  print_endline (output_file config)

(** [main_execute config] executes OCamlgrapher using [config]. *)
let main_execute (config : Config.t) =
  let x_b, y_b = (x_bounds config, y_bounds config) in
  let processed =
    equations config
    |> List.map (fun t -> process_equation t (steps config) x_b y_b)
  in
  match command config with
  | Graph -> make_graph config processed
  | Points -> List.iter print_points processed
  | Extrema -> List.iter print_extrema processed
  | Roots -> List.iter print_roots processed

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
      main_execute cfg

let () = main ()
