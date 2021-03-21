(** Implementation of module [Config].*)

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

let rec starts_with str sub =
  try String.sub str 0 (String.length sub) = sub
  with Invalid_argument s -> false

(** [ch @^ str] concatenates [str] onto [ch].*)
let ( @^ ) ch str = String.make 1 ch ^ str

let chop str n =
  try String.sub str n (String.length str - n) with _ -> ""

let help () =
  Printf.eprintf
    "ocamlgrapher\n\
     Usage: ocamlgrapher <options> <equation>\n\
     Example Usage: ocamlgrapher -g -o my_graph.png \"y=2x^2-4ln(x)\"\n\
     Options are:\n\
     \t\"-g\", \"--graph\": Generate a visual graph of the provided \
     equation and save it to the specified output file, or \
     \"graph.svg\" if none was specified. Default mode. \n\
     \t\"-o\", \"--output\": Filename to save output to.\n\
     \t\"-x <float>\", \"--xmin <float>\": Set the low end of the \
     domain.\n\
     \t\"-X <float>\", \"--xmax <float>\": Set the high end of the \
     domain.\n\
     \t\"-y <float>\", \"--ymin <float>\": Set the low end of the range.\n\
     \t\"-Y <float>\", \"--ymax <float>\": Set the high end of the \
     range.\n\
     \t\"-h\", \"--help\": Print this help dialog and don't perform \
     any actual work. \n";
  exit 0

let arg_parse_long_param (arg : string) (tail : string list) =
  match (String.split_on_char '=' arg, tail) with
  | [ opt ], param :: new_tail -> Ok (opt, param, new_tail)
  | opt :: f :: r, _ ->
      Ok (opt, List.fold_left (fun a b -> a ^ '=' @^ b) f r, tail)
  | _ -> Error ("\"" ^ arg ^ "\": malformed argument")

let rec arg_parse_long (arg : string) (tail : string list) (cfg : t) =
  match arg with
  | "graph" -> Ok ({ cfg with command = Graph }, tail)
  | "points" -> Ok ({ cfg with command = Points }, tail)
  | "roots" -> Ok ({ cfg with command = Roots }, tail)
  | "extrema" -> Ok ({ cfg with command = Extrema }, tail)
  | "help" -> help ()
  | x -> (
      match arg_parse_long_param x tail with
      | Ok (arg, str, new_tail) -> (
          if arg = "output" then
            Ok ({ cfg with output_file = Some str }, new_tail)
          else
            let x1, x2 = cfg.domain in
            let y1, y2 = cfg.range in
            match (arg, float_of_string_opt str) with
            | "xmin", Some flt ->
                Ok ({ cfg with domain = (flt, x2) }, new_tail)
            | "xmax", Some flt ->
                Ok ({ cfg with domain = (x1, flt) }, new_tail)
            | "ymin", Some flt ->
                Ok ({ cfg with range = (flt, y2) }, new_tail)
            | "ymax", Some flt ->
                Ok ({ cfg with range = (y1, flt) }, new_tail)
            | "xmin", None | "xmax", None | "ymin", None | "ymax", None
              ->
                Error
                  ( "Could not convert parameter for " ^ arg
                  ^ " to string" )
            | _ -> Error ("Option " ^ arg ^ " is unknown") )
      | Error e -> Error e )

let arg_parse_short_param (rest : string) (tail : string list) =
  if rest <> "" then Some (rest, tail)
  else
    match tail with
    | [] -> None
    | next :: next_tail -> Some (next, next_tail)

let rec arg_parse_short (arg : string) (tail : string list) (cfg : t) =
  if arg = "" then Ok (cfg, tail)
  else
    let rest = chop arg 1 in
    match arg.[0] with
    (* Check all the zero-parameter options *)
    | 'g' -> arg_parse_short rest tail { cfg with command = Graph }
    | 'p' -> arg_parse_short rest tail { cfg with command = Points }
    | 'r' -> arg_parse_short rest tail { cfg with command = Roots }
    | 'e' -> arg_parse_short rest tail { cfg with command = Extrema }
    | 'h' -> help ()
    (* Check the parametric options *)
    | c -> (
        match arg_parse_short_param rest tail with
        | None -> Error ("Unknown zero-parameter option " ^ c @^ "")
        | Some (pstr, new_tail) -> (
            let x1, x2 = cfg.domain in
            let y1, y2 = cfg.range in
            match (c, float_of_string_opt pstr) with
            | 'o', _ ->
                Ok ({ cfg with output_file = Some pstr }, new_tail)
            | 'x', Some flt ->
                Ok ({ cfg with domain = (flt, x2) }, new_tail)
            | 'X', Some flt ->
                Ok ({ cfg with domain = (x1, flt) }, new_tail)
            | 'y', Some flt ->
                Ok ({ cfg with range = (flt, y2) }, new_tail)
            | 'Y', Some flt ->
                Ok ({ cfg with range = (y1, flt) }, new_tail)
            | 'x', None | 'X', None | 'y', None | 'Y', None ->
                Error
                  ( "Could not convert parameter for " ^ c
                  @^ " to string" )
            | _ -> Error ("Option " ^ c @^ " is unknown") ) )

let rec arg_step (eq : string option) (cfg : t) = function
  | [] -> (
      (* make sure we have parsed an equation *)
      match eq with
      | None -> Error "No equation provided"
      | Some equation -> Ok { cfg with equation } )
  | str :: t -> (
      let multi_eq () = Error "Multiple equations provided" in
      let opt_parse op chopcount =
        match op (chop str chopcount) t cfg with
        | Ok (new_cfg, new_tail) -> arg_step eq new_cfg new_tail
        | Error e -> Error e
      in
      if str = "-" then
        (* Convention: "-" reads from stdin *)
        if eq <> None then multi_eq ()
        else arg_step (Some (read_line ())) cfg t
      else if str = "--" then
        (* Convention: "--" treats the following argument as input. *)
        match t with
        | [ last ] when eq = None -> arg_step (Some last) cfg []
        | _ -> multi_eq ()
      else if starts_with str "--" then opt_parse arg_parse_long 2
      else if starts_with str "-" then opt_parse arg_parse_short 1
      else
        match eq with
        | None -> arg_step (Some str) cfg t
        | Some _ -> multi_eq () )

let from_argv2 argv default_domain default_range =
  match Array.to_list argv with
  | [] ->
      raise (Invalid_argument "argv must contain at least one entry")
  | [ _ ] -> Error "No arguments"
  | _ :: args -> (
      match
        arg_step None
          {
            command = Graph;
            equation = "";
            domain = default_domain;
            range = default_range;
            output_file = None;
          }
          args
      with
      | Ok cfg ->
          if
            fst cfg.domain <= snd cfg.domain
            && fst cfg.range <= snd cfg.range
          then Ok cfg
          else Error "Invalid domain and/or range"
      | Error e -> Error e )

type parse_rule_t =
  | Flag of (string * char option)
  | Opt of (string * char option)

(** [parse_argv_t] represents the parsed command line. It is
    program-and-meaning agnostic.

    [name] is the name of the binary being executed.

    [args] is the list of arguments on the command line. An "argument"
    is defined as anything that is not an option and isn't a parameter
    of an option. By convention, if the token "--" is encountered on the
    command line, all tokens afterwards are interpreted as options.
    Similarly, if the token "-" is encountered on the command line, each
    line of stdin is interpreted as an argument. This list is in
    left-to-right order with respect to the command line.

    [flags] is the list of all flags passed on the command line, where a
    flag is defined as an option that takes no parameter. [flags] is in
    right-to-left order; that is, the rightmost flags on the command
    line are the leftmost flags in [flags]. If a flag has a long and
    short form, the long form will be the one present in [flags],
    regardless of which form appears on the command line. For example,
    if you ran `./binary --no-color --confirm`, [flags] would be
    [\["confirm";"no-color"\]].

    [opts] is an association list. Each key in the association list
    represents a parameter-taking option in the parse rules -- that is,
    every option in the rules is represented. Each key is unique, and if
    the option has long and short forms, then the long form is the key,
    regardless of what form(s) appeared on the command line. The order
    of the keys are unspecified. The value of each key is a list of all
    the parameters assigned to the key's option on the command line in
    right-to-left order. If the value is an empty list, then the option
    did not appear on the command line. For example, if you ran
    `./binary --open=sesame --debug 5 -o door`, with `-o` as a shortform
    for `--open`, [opts] could be
    [\[("open", \["door"; "sesame"\]); ("debug", \["5"\])\]] *)
type parse_argv_t = {
  name : string;
  args : string list;
  flags : string list;
  opts : (string * string list) list;
}

type worker_rules_t = {
  short_flags : (char * string) list;
  long_flags : string list;
  short_opts : (char * string) list;
  long_opts : string list;
}

let worker_rules (rules : parse_rule_t list) =
  let filter_rules f = List.filter_map f rules in
  {
    short_flags =
      filter_rules (function
        | Flag (long, Some s) -> Some (s, long)
        | _ -> None);
    short_opts =
      filter_rules (function
        | Opt (long, Some s) -> Some (s, long)
        | _ -> None);
    long_flags =
      filter_rules (function Flag (long, _) -> Some long | _ -> None);
    long_opts =
      filter_rules (function Opt (long, _) -> Some long | _ -> None);
  }

(** Read the entirety of [ch] and return a list of strings, where each
    entry is a line of the file (without the newline character). The
    returned list is in the reverse order from how they appear in the
    file.*)
let rec read_lines (ch : in_channel) : string list =
  let rec step (acc : string list) =
    try step (input_line ch :: acc) with End_of_file -> acc
  in
  step []

let replace_assoc key new_val =
  List.map (fun (k, v) -> (k, if k = key then new_val else v))

let join connector a b = a ^ connector ^ b

let rec parse_worker (rules : worker_rules_t) (acc : parse_argv_t) =
  function
  | "--" :: t -> Ok { acc with args = t @ acc.args }
  | "-" :: t ->
      parse_worker rules
        { acc with args = List.rev_append (read_lines stdin) acc.args }
        t
  | token :: t ->
      if starts_with token "--" then
        match String.split_on_char '=' token with
        (* it must be a long flag *)
        | [ arg ] ->
            if List.mem arg rules.long_flags then
              parse_worker rules { acc with flags = arg :: acc.flags } t
            else Error ("Unrecognized option \"" ^ arg ^ "\"")
        (* it must be a long option *)
        | arg :: param_lst ->
            if List.mem arg rules.long_opts then
              let param = List.fold_left (join "=") "" param_lst in
              failwith "Unimplemented"
            else Error ("Unrecognized option \"" ^ arg ^ "\"")
        | [] -> failwith "Impossible state"
      else if starts_with token "-" then Error "uh oh"
      else parse_worker rules { acc with args = acc.args @ [ token ] } t
  | [] -> Ok acc

let parse_argv (rules : parse_rule_t list) = function
  | [] -> raise (Invalid_argument "argv must have at least one entry")
  | name :: tail ->
      let worker_rules = worker_rules rules in
      let initial =
        {
          name;
          args = [];
          flags = [];
          (* invariant: [opts] contains a key for each opt rule *)
          opts = List.map (fun key -> (key, [])) worker_rules.long_opts;
        }
      in
      parse_worker worker_rules initial tail

let extract_equation = function
  | [ eq ] -> Ok eq
  | [] -> Error "No equation provided (provide exactly one)"
  | _ -> Error "Multiple equations provided (provide exactly one)"

let extract_output opts =
  match List.assoc "output" opts with
  | [] -> Ok None
  | [ filename ] -> Ok (Some filename)
  | _ -> Error "Multiple output files specified"

let extract_bounds opts (default_min, default_max) dimension var =
  try
    let min =
      match List.assoc (var @^ "min") opts with
      | [] -> default_min
      | v :: _ -> float_of_string v
    in
    let max =
      match List.assoc (var @^ "max") opts with
      | [] -> default_max
      | v :: _ -> float_of_string v
    in
    if min > max then
      Error ("Minimum bound on " ^ dimension ^ " > than maximum bound.")
    else Ok (min, max)
  with Invalid_argument _ -> Error "Bounds must be floats"

(** [extract_command flags] gets the command from the flag list [flags],
    where [flags] is defined in the same way as [prase_argv_t.flags].
    Requires: the only flags in [flags] are "graph", "roots", "points",
    and "extrema". *)
let extract_command = function
  | "roots" :: _ -> Roots
  | "points" :: _ -> Points
  | "extrema" :: _ -> Extrema
  | _ -> Graph

exception Result_bad_assume of string

let assume_res x =
  match x with Ok x -> x | Error s -> raise (Result_bad_assume s)

let from_argv argv default_domain default_range =
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
      (Array.to_list argv)
  with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" res.flags then help ()
        (* what is with this formatting man *)
      else
        try
          Ok
            {
              command = extract_command res.flags;
              equation = assume_res (extract_equation res.args);
              domain =
                assume_res
                  (extract_bounds res.opts default_domain "domain" 'x');
              range =
                assume_res
                  (extract_bounds res.opts default_range "range" 'y');
              output_file = assume_res (extract_output res.opts);
            }
        with Result_bad_assume s -> Error s )

let equation cfg = cfg.equation

let domain cfg = cfg.domain

let range cfg = cfg.range

let command cfg = cfg.command

let output_file cfg = cfg.output_file

let to_string cfg =
  let a, b = cfg.domain in
  let c, d = cfg.range in
  let verb =
    match cfg.command with
    | Graph -> "Graph"
    | Points -> "List points satisfying"
    | Roots -> "List the roots of"
    | Extrema -> "List the extrema of"
  in
  Printf.sprintf "%s %s with x in [%f, %f] and y in [%f, %f]" verb
    cfg.equation a b c d
  ^
  match cfg.output_file with
  | None -> ""
  | Some f -> ", outputting to " ^ f
