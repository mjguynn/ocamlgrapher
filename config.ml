(** Implementation of module [Config].*)
open Cmdline

(* Unfortunately, I must duplicate this code from the MLI.*)
type command_t =
  | Graph
  | Points
  | Roots
  | Extrema

(** RI: Let [domain=(a,b)]. Then [cfg] is only meaningful if [x<=y]. Let
    [range=(c,d)]. Then [cfg] is only meaningful if [c<=d].*)
type t = {
  command : command_t;
  equation : string;
  domain : float * float;
  range : float * float;
  output_file : string option;
}

(** [Bad_assume s] represents that assuming a [Result] was [Ok] was
    incorrect, because that result was actually an [Error s].*)
exception Bad_assume of string

(** [assume_res result] assumes that [result] is [Ok] and returns the
    contained value. If [result] is [Error s], throws [Bad_assume s].*)
let assume_res x =
  match x with Ok x -> x | Error s -> raise (Bad_assume s)

(** [help errc] prints program help, then exits with a return code of
    [errc]. *)
let help errc =
  Printf.eprintf
    "ocamlgrapher\n\
     Usage: ocamlgrapher <options> <equation>\n\
     Example Usage: ocamlgrapher -g -o my_graph.png \"y=2x^2-4ln(x)\"\n\
     Options are:\n\
     \t\"-g\", \"--graph\": Generate a visual graph of the provided \
     equation and save it to the specified output file, or \
     \"graph.svg\" if none was specified. Default mode. \n\
     \t\"-o\", \"--output\": Filename to save output to.\n\
     \t\"-x <float>\", \"--xmin=<float>\": Set the low end of the \
     domain.\n\
     \t\"-X <float>\", \"--xmax=<float>\": Set the high end of the \
     domain.\n\
     \t\"-y <float>\", \"--ymin=<float>\": Set the low end of the range.\n\
     \t\"-Y <float>\", \"--ymax=<float>\": Set the high end of the \
     range.\n\
     \t\"-h\", \"--help\": Print this help dialog and don't perform \
     any actual work. \n";
  exit errc

let extract_equation = function
  | [ eq ] -> Ok eq
  | [] -> Error "No equation provided (provide exactly one)"
  | _ -> Error "Multiple equations provided (provide exactly one)"

let extract_output opts =
  match List.assoc "output" opts with
  | [] -> Ok None
  | [ filename ] -> Ok (Some filename)
  | _ -> Error "Multiple output files specified"

let extract_bounds opts (min, max) dimension var =
  let float_opt suffix default =
    match List.assoc (var ^ suffix) opts with
    | [] -> default
    | v :: _ -> float_of_string v
  in
  try
    let min, max = (float_opt "min" min, float_opt "max" max) in
    if min > max then
      Error ("Minimum bound on " ^ dimension ^ " > than maximum bound.")
    else Ok (min, max)
  with Failure _ -> Error "Bounds must be floats"

(** [extract_command flags] gets the command from the flag list [flags],
    where [flags] is defined in the same way as [prase_argv_t.flags].
    Requires: the only flags in [flags] are "graph", "roots", "points",
    and "extrema". *)
let extract_command = function
  | "roots" :: _ -> Roots
  | "points" :: _ -> Points
  | "extrema" :: _ -> Extrema
  | _ -> Graph

let from_argv argv d r =
  match
    parse_argv
      [
        Flag ("help", Some 'h');
        Flag ("roots", Some 'r');
        Flag ("graph", Some 'g');
        Flag ("points", Some 'p');
        Flag ("extrema", Some 'e');
        Opt ("output", Some 'o');
        Opt ("xmin", Some 'x');
        Opt ("xmax", Some 'X');
        Opt ("ymin", Some 'y');
        Opt ("ymax", Some 'Y');
      ]
      argv
  with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" res.flags then help 0;
      try
        Ok
          {
            command = extract_command res.flags;
            equation = assume_res (extract_equation res.args);
            domain = assume_res (extract_bounds res.opts d "domain" "x");
            range = assume_res (extract_bounds res.opts r "range" "y");
            output_file = assume_res (extract_output res.opts);
          }
      with Bad_assume s -> Error s )

let equation cfg = cfg.equation

let domain cfg = cfg.domain

let range cfg = cfg.range

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
  Printf.sprintf "%s %s with x in [%f, %f] and y in [%f, %f]" verb
    cfg.equation a b c d
  ^
  match cfg.output_file with
  | None -> ""
  | Some f -> ", outputting to " ^ f
