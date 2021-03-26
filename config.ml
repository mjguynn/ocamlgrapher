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

let help errc =
  Printf.eprintf
    "Usage: ocamlgrapher <options> <equation>\n\
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

(** [extract_equation cmdline]: if there is exactly one free argument
    (an equation) on [cmdline], returns that with an [Ok] result;
    otherwise, returns an [Error] result with an error message.*)
let extract_equation cmdline =
  match arguments cmdline with
  | [ eq ] -> Ok eq
  | [] -> Error "No equation provided (provide exactly one)"
  | _ -> Error "Multiple equations provided (provide exactly one)"

(** [extract_output cmdline]: if one output file [f] was specified on
    [cmdline], it returns [Some f] with an [Ok] result; if none were
    specified, it returns [None] with an [Ok] result; otherwise, returns
    an [Error] result with an error message.*)
let extract_output cmdline =
  match List.assoc "output" (options cmdline) with
  | [] -> Ok None
  | [ filename ] -> Ok (Some filename)
  | _ -> Error "Multiple output files specified"

(** [extract_bounds cmdline (min, max) dim var]: returns an [Ok] result
    containing the pair (a, b) bounding [dim] as specified on [cmdline].
    [dim] is the name of of the dimension (ex. "domain", "range") and
    [var] is the name of is the variable on that dimension (ex. "x",
    "y"). If a minimum bound was not specified on the command line, it
    defaults to [min]; if a maximum bound was not specified on the
    command line, it defaults to [max]. If the minimum bound ends up
    being less than the maximum bound, an [Error] with an error message
    is returned instead.*)
let extract_bounds cmdline (default_min, default_max) dimension =
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
      ( float_opt "-min" default_min |> assume_res,
        float_opt "-max" default_max |> assume_res )
    in
    if min <= max then Ok (min, max)
    else Error ("Minimum bound on " ^ dimension ^ " > maximum bound.")
  with Bad_assume s -> Error s

(** [extract_command cmdline] identitifies and returns the command from
    command line [cmdline]. If none is found, returns [Graph].

    Requires: the only flags in [cmdline] are "graph", "roots",
    "points", and "extrema". *)
let extract_command cmdline =
  match flags cmdline with
  | "roots" :: _ -> Roots
  | "points" :: _ -> Points
  | "extrema" :: _ -> Extrema
  | _ -> Graph

let from_cmdline argv ic d r =
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
      ]
      ic argv
  with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" (flags res) then help 0;
      try
        Ok
          {
            command = extract_command res;
            equation = assume_res (extract_equation res);
            domain = assume_res (extract_bounds res d "domain");
            range = assume_res (extract_bounds res r "range");
            output_file = assume_res (extract_output res);
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
