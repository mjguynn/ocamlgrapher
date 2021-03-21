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

(** [starts_with str sub] returns whether [str] starts with [sub].*)
let rec starts_with str sub =
  try String.sub str 0 (String.length sub) = sub
  with Invalid_argument s -> false

(** [ch @^ str] concatenates [str] onto [ch].*)
let ( @^ ) ch str = String.make 1 ch ^ str

(** [drop str n] returns [str] without the first [n] characters. If
    [str] is shorter than [n] characters, the empty string is returned.*)
let drop str n =
  try String.sub str n (String.length str - n) with _ -> ""

(** [prepend_assoc key x] takes an association list where keys are
    mapped to a list. Then, for all keys = [key], [x] is cons-ed onto
    the list.*)
let prepend_assoc key x =
  List.map (fun (k, v) -> (k, if k = key then x :: v else v))

(** [join c a b] returns [a ^ c ^ b]. *)
let join connector a b = a ^ connector ^ b

(** [read_lines] returns a list of strings, where each entry is a line
    in [ch] (without the newline character). The list contains an entry
    for each line in [ch] and with the same order. Requies: [ch] is
    readable.*)
let rec read_lines (ch : in_channel) : string list =
  let rec step (acc : string list) =
    try step (input_line ch :: acc) with End_of_file -> acc
  in
  List.rev (step [])

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

(** [parse_rule_t] represents a rule that should be respected while
    parsing. A [Flag] rule represents an option that takes no
    parameters. A [Opt] rule represents an option that takes no
    parameters, aka, a "parameterized option". The first element in the
    parse rule is a non-zero string representing the GNU long option
    name of the option, and the second element is an optional character
    that can be used as a short option. For example, if you wanted to
    use "-h" or "--help", you would add an [Opt ("help", Some 'h')].*)
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
    right-to-left order with respect to the command line.

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
    `./binary --open=sesame --debug=5 -o door`, with `-o` as a shortform
    for `--open`, [opts] could be
    [\[("open", \["door"; "sesame"\]); ("debug", \["5"\])\]] *)
type parse_argv_t = {
  name : string;
  args : string list;
  flags : string list;
  opts : (string * string list) list;
}

let rec parse_long_arg token rules args acc =
  let has_rule f = List.exists f rules in
  match String.split_on_char '=' token with
  | [] -> raise (Invalid_argument "Impossible state")
  | [ arg ] ->
      if
        has_rule (function
          | Flag (f, _) when f = arg -> true
          | _ -> false)
      then parse_worker rules args { acc with flags = arg :: acc.flags }
      else Error ("Unrecognized long flag \"" ^ arg ^ "\"")
  | arg :: param_head :: param_tail ->
      if
        has_rule (function
          | Opt (f, _) when f = arg -> true
          | _ -> false)
      then
        let param = List.fold_left (join "=") param_head param_tail in
        parse_worker rules args
          { acc with opts = prepend_assoc arg param acc.opts }
      else Error ("Unrecognized long option \"" ^ arg ^ "\"")

and parse_short_arg token rules args acc =
  if token = "" then parse_worker rules args acc
  else
    let rest = drop token 1 in
    let cur = token.[0] in
    match
      List.find_opt
        (function
          | Flag (_, Some s) -> s = cur
          | Opt (_, Some s) -> s = cur
          | _ -> false)
        rules
    with
    | None -> Error ("Unrecognized short flag/option \"" ^ cur @^ "\"")
    | Some (Flag (arg, _)) ->
        parse_short_arg rest rules args
          { acc with flags = arg :: acc.flags }
    | Some (Opt (arg, _)) -> (
        match args with
        | _ when rest <> "" ->
            parse_worker rules args
              { acc with opts = prepend_assoc arg rest acc.opts }
        | h :: t ->
            parse_worker rules t
              { acc with opts = prepend_assoc arg h acc.opts }
        | [] ->
            Error ("Short option \"" ^ arg ^ "\" requires one parameter")
        )

and parse_worker
    (rules : parse_rule_t list)
    (args : string list)
    (acc : parse_argv_t) =
  match args with
  | "--" :: t -> Ok { acc with args = List.rev_append t acc.args }
  | "-" :: t ->
      let new_acc =
        { acc with args = List.append (read_lines stdin) acc.args }
      in
      parse_worker rules t new_acc
  | token :: t ->
      if starts_with token "--" then
        parse_long_arg (drop token 2) rules t acc
      else if starts_with token "-" then
        parse_short_arg (drop token 1) rules t acc
      else parse_worker rules t { acc with args = token :: acc.args }
  | [] -> Ok acc

let parse_argv (rules : parse_rule_t list) = function
  | [] -> raise (Invalid_argument "argv must have at least one entry")
  | name :: args ->
      let initial =
        {
          name;
          args = [];
          flags = [];
          (* invariant: [opts] contains a key for each opt rule *)
          opts =
            List.filter_map
              (function Opt (key, _) -> Some (key, []) | _ -> None)
              rules;
        }
      in
      parse_worker rules args initial

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
    match List.assoc (var @^ suffix) opts with
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
      (Array.to_list argv)
  with
  | Error e -> Error e
  | Ok res -> (
      if List.mem "help" res.flags then help 0;
      try
        Ok
          {
            command = extract_command res.flags;
            equation = assume_res (extract_equation res.args);
            domain = assume_res (extract_bounds res.opts d "domain" 'x');
            range = assume_res (extract_bounds res.opts r "range" 'y');
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
