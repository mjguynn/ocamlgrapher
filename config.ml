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
        | [ last ] when eq <> None -> arg_step (Some last) cfg []
        | _ -> multi_eq ()
      else if starts_with str "--" then opt_parse arg_parse_long 2
      else if starts_with str "-" then opt_parse arg_parse_short 1
      else
        match eq with
        | None -> arg_step (Some str) cfg t
        | Some _ -> multi_eq () )

let from_argv argv default_domain default_range =
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
