(** Implementation of module [Config].*)
open Cmdline

(* Unfortunately, I must duplicate this code from the MLI.*)
type command_t =
  | Graph
  | Points
  | Roots
  | Extrema

(** AF: Let [(x1, x2)=x_bounds] and [(y1, y2)=y_bounds]. Then an
    instance of [t] represents a user's desires to execute [command] on
    the equations in [equations] (in ASCII math notation) within a
    region spanning [x1..x2] on the X-axis and [y1..y2] on the Y-axis,
    with a precision of [steps]. [output_file] represents the file which
    shall recieve relevant program output.

    RI: [cfg] is only meaningful if [x1<x2], [y1<y2], and [steps>=1].*)
type t = {
  command : command_t;
  equations : string list;
  x_bounds : float * float;
  y_bounds : float * float;
  steps : int;
  output_file : string;
}

(** [Bad_assume s] represents that assuming a [Result] was [Ok] was
    incorrect, because that result was actually an [Error s].*)
exception Bad_assume of string

(** [assume_ok result] assumes that [result] is [Ok] and returns the
    contained value. If [result] is [Error s], throws [Bad_assume s].*)
let assume_ok x =
  match x with Ok x -> x | Error s -> raise (Bad_assume s)

(** [cmdline_info] is the authoritative list of command-line options and
    their help text.*)
let cmdline_info =
  [
    ( Flag ("help", Some 'h'),
      "Print this help dialog and don't perform any actual work." );
    ( Flag ("roots", Some 'r'),
      "List the roots of the equation(s) within the given bounds." );
    ( Flag ("graph", Some 'g'),
      "Create a visual graph of the equation(s), and print the \
       filename of the resulting file." );
    (Flag ("points", Some 'p'), "List some points on the equation(s).");
    ( Flag ("extrema", Some 'e'),
      "List the extrema of the equation(s) within the given bounds." );
    (Opt ("output", Some 'o'), "The name of the graph output file.");
    (Opt ("x-min", None), "Set the minimum bound on the X axis.");
    (Opt ("x-max", None), "Set the maximum bound on the X axis.");
    (Opt ("y-min", None), "Set the minimum bound on the Y axis.");
    (Opt ("y-max", None), "Set the maxmimum bound on the Y axis.");
    ( Opt ("quality", Some 'q'),
      "Set the number of \"steps\" used to analyze the function and/or \
       draw its graph. Higher is better." );
  ]

let help errc =
  let open Common in
  let header s = style stderr "96" ^ s ^ style stderr "0" in
  let grey s = style stderr "38;2;140;140;140" ^ s ^ style stderr "0" in
  Printf.eprintf "%s"
    ( header "Usage: " ^ "./ocamlgrapher.byte "
    ^ grey "<options> <equations>\n" );
  Printf.eprintf "%s"
    ( header "Example: "
    ^ "./ocamlgrapher.byte -g -o my_graph.png \"y=2x^2-4ln(x)\"\n" );
  Printf.eprintf "%s" (header "Options: \n");
  let build_name long short append_long append_short =
    Printf.sprintf "--%s%s%s" long append_long
      ( match short with
      | None -> ""
      | Some c -> Printf.sprintf ", -%c%s" c append_short )
  in
  cmdline_info
  |> List.iter (fun (rule, desc) ->
         let name =
           match rule with
           | Flag (f, s) -> build_name f s "" ""
           | Opt (o, s) -> build_name o s "=<...>" " <...>"
         in
         Printf.eprintf "\t%-28s: %s\n" name (grey desc));
  exit errc

(** [extract_equations cmdline]: If >= 1 equations on [cmdline], returns
    those equations in the order they appeared on the command line.
    Otherwise, returns an [Error] result with an error message.*)
let extract_equations cmdline =
  match arguments cmdline with
  | [] -> Error "No equation provided (provide exactly one)"
  | eqs -> Ok (List.rev eqs)

(** [extract_output cmdline]: if one output file [f] was specified on
    [cmdline], it returns [Some f] with an [Ok] result; if none were
    specified, it returns [None] with an [Ok] result; otherwise, returns
    an [Error] result with an error message.*)
let extract_output cmdline default_out =
  match List.assoc "output" (options cmdline) with
  | [] -> Ok default_out
  | [ filename ] -> Ok filename
  | _ -> Error "Multiple output files specified"

(** [extract_steps cmdline default]: returns the user specified number
    of steps on the command line, if it exists; otherwise, returns the
    default value. If the user typed something but it wasn't an integer
    OR it was < 1, return an error message instead.*)
let extract_steps cmdline default_steps =
  match List.assoc "quality" (options cmdline) with
  | [] ->
      assert (default_steps >= 1);
      Ok default_steps
  | [ s ] -> (
      match int_of_string_opt s with
      | Some i when i >= 1 -> Ok i
      | _ -> Error "Quality must be an integer >= 1" )
  | _ -> Error "Multiple qualities specified"

(** [extract_bounds cmdline (min, max) dim]: returns an [Ok] result
    containing the pair (a, b) bounding [dim] as specified on [cmdline].
    [dim] is the name of of the dimension (ex. "x", "y"). If a minimum
    bound was not specified on the command line, it defaults to [min];
    if a maximum bound was not specified on the command line, it
    defaults to [max]. If the minimum bound ends up being less than the
    maximum bound, or one or both bounds are inf or nan, an [Error] with
    an error message is returned instead.*)
let extract_bounds cmdline (default_min, default_max) dimension =
  let valid_float flt =
    let c = classify_float flt in
    c <> FP_infinite && c <> FP_nan
  in
  let float_opt suffix default =
    match List.assoc (dimension ^ suffix) (options cmdline) with
    | [] -> Ok default
    | [ v ] ->
        float_of_string_opt v
        |> Option.to_result ~none:(dimension ^ " bound must be floats")
    | _ -> Error ("Multiple bounds specified on " ^ dimension)
  in
  try
    let min, max =
      ( float_opt "-min" default_min |> assume_ok,
        float_opt "-max" default_max |> assume_ok )
    in
    if Bool.not (valid_float min && valid_float max) then
      Error ("Bounds on " ^ dimension ^ " are NaN or infinite.")
    else if min > max then
      Error ("Minimum bound on " ^ dimension ^ " > maximum bound.")
    else Ok (min, max)
  with Bad_assume s -> Error s

(** [extract_command cmdline] identitifies and returns the command from
    command line [cmdline]. If none is found, returns [Ok Graph]. If
    multiple are found, returns [Error s] where s is a descriptive error
    message.

    Requires: the only flags in [cmdline] are "graph", "roots",
    "points", and "extrema". *)
let extract_command cmdline =
  match flags cmdline with
  | [ "roots" ] -> Ok Roots
  | [ "points" ] -> Ok Points
  | [ "extrema" ] -> Ok Extrema
  | [ "graph" ] | [] -> Ok Graph
  | _ -> Error "Only specify one mode (graph, roots, points, extrema)"

let from_cmdline xs ys s o ic argv =
  match parse_cmdline (List.map fst cmdline_info) ic argv with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" (flags res) then help 0;
      try
        Ok
          {
            command = extract_command res |> assume_ok;
            equations = extract_equations res |> assume_ok;
            x_bounds = extract_bounds res xs "x" |> assume_ok;
            y_bounds = extract_bounds res ys "y" |> assume_ok;
            steps = extract_steps res s |> assume_ok;
            output_file = extract_output res o |> assume_ok;
          }
      with Bad_assume s -> Error s )

let equations cfg = cfg.equations

let x_bounds cfg = cfg.x_bounds

let y_bounds cfg = cfg.y_bounds

let steps cfg = cfg.steps

let command cfg = cfg.command

let output_file cfg = cfg.output_file

let to_string cfg =
  let verb =
    match cfg.command with
    | Graph -> "Graph"
    | Points -> "List points satisfying"
    | Roots -> "List the roots of"
    | Extrema -> "List the extrema of"
  in
  let a, b = cfg.x_bounds in
  let c, d = cfg.y_bounds in
  let equation_str =
    match cfg.equations with
    | [] -> failwith "Impossible - RI violated"
    | [ e1 ] -> e1
    | e1 :: t -> List.fold_left (fun a b -> a ^ ", " ^ b) e1 t
  in
  Printf.sprintf
    "%s %s with x in [%f, %f] and y in [%f, %f], using %i steps" verb
    equation_str a b c d cfg.steps
  ^
  if cfg.command = Graph then ", outputting to " ^ cfg.output_file
  else ""
