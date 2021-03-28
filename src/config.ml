(** Implementation of module [Config].*)
open Cmdline

(* Unfortunately, I must duplicate this code from the MLI.*)
type command_t =
  | Graph
  | Points
  | Roots
  | Extrema

(** RI: Let [domain=(a,b)]. Then [cfg] is only meaningful if [x<=y]. Let
    [range=(c,d)]. Then [cfg] is only meaningful if [c<=d]. Also, [cfg]
    is only meaningful if [steps>=1].*)
type t = {
  command : command_t;
  equations : string list;
  domain : float * float;
  range : float * float;
  steps : int;
  output_file : string option;
}

(** [Bad_assume s] represents that assuming a [Result] was [Ok] was
    incorrect, because that result was actually an [Error s].*)
exception Bad_assume of string

(** [assume_ok result] assumes that [result] is [Ok] and returns the
    contained value. If [result] is [Error s], throws [Bad_assume s].*)
let assume_ok x =
  match x with Ok x -> x | Error s -> raise (Bad_assume s)

let help errc =
  Printf.eprintf
    "Usage: ocamlgrapher <options> <equations>\n\
     Example: ocamlgrapher -g -o my_graph.png \"y=2x^2-4ln(x)\"\n\
     Options:\n\
     \t\"-g\", \"--graph\": Generate a visual graph of the provided \
     equation and save it to the specified output file, or \
     \"graph.svg\" if none was specified. Default mode. \n\
     \t\"-o\", \"--output\": Filename to save output to.\n\
     \"--domain-min=<float>\": Set the low end of the domain.\n\
     \t\"--domain-max=<float>\": Set the high end of the domain.\n\
     \t\"--range-min=<float>\": Set the low end of the range.\n\
     \t\"--range-max=<float>\": Set the high end of the range.\n\
     \t\"-h\", \"--help\": Print this help dialog and don't perform \
     any actual work. \n";
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
let extract_output cmdline =
  match List.assoc "output" (options cmdline) with
  | [] -> Ok None
  | [ filename ] -> Ok (Some filename)
  | _ -> Error "Multiple output files specified"

let extract_steps cmdline default_steps =
  match List.assoc "steps" (options cmdline) with
  | [] ->
      assert (default_steps >= 1);
      Ok default_steps
  | [ s ] -> (
      match int_of_string_opt s with
      | Some i when i >= 1 -> Ok i
      | _ -> Error "Number of steps must be an integer >= 1" )
  | _ -> Error "Multiple step sizes specified"

(** [extract_bounds cmdline (min, max) dim var]: returns an [Ok] result
    containing the pair (a, b) bounding [dim] as specified on [cmdline].
    [dim] is the name of of the dimension (ex. "domain", "range") and
    [var] is the name of is the variable on that dimension (ex. "x",
    "y"). If a minimum bound was not specified on the command line, it
    defaults to [min]; if a maximum bound was not specified on the
    command line, it defaults to [max]. If the minimum bound ends up
    being less than the maximum bound, or one or both bounds are inf or
    nan, an [Error] with an error message is returned instead.*)
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
    command line [cmdline]. If none is found, returns [Graph].

    Requires: the only flags in [cmdline] are "graph", "roots",
    "points", and "extrema". *)
let extract_command cmdline =
  match flags cmdline with
  | [ "roots" ] -> Ok Roots
  | [ "points" ] -> Ok Points
  | [ "extrema" ] -> Ok Extrema
  | [ "graph" ] | [] -> Ok Graph
  | _ -> Error "Only specify one mode (graph, roots, points, extrema)"

let from_cmdline d r s ic argv =
  match
    parse_cmdline
      [
        Flag ("help", Some 'h');
        Flag ("roots", Some 'r');
        Flag ("graph", Some 'g');
        Flag ("points", Some 'p');
        Flag ("extrema", Some 'e');
        Opt ("output", Some 'o');
        Opt ("domain-min", None);
        Opt ("domain-max", None);
        Opt ("range-min", None);
        Opt ("range-max", None);
        Opt ("steps", None);
      ]
      ic argv
  with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" (flags res) then help 0;
      try
        Ok
          {
            command = extract_command res |> assume_ok;
            equations = extract_equations res |> assume_ok;
            domain = extract_bounds res d "domain" |> assume_ok;
            range = extract_bounds res r "range" |> assume_ok;
            steps = extract_steps res s |> assume_ok;
            output_file = extract_output res |> assume_ok;
          }
      with Bad_assume s -> Error s )

let equations cfg = cfg.equations

let domain cfg = cfg.domain

let range cfg = cfg.range

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
  let a, b = cfg.domain in
  let c, d = cfg.range in
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
  match cfg.output_file with
  | None -> ""
  | Some f -> ", outputting to " ^ f
