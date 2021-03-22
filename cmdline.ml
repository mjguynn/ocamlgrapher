(** Implementation of module [Cmdline].*)

(* Unfortunately, I have to duplicate some code from the interface. *)
(* DUPLICATED CODE BEGIN *)
type parse_rule_t =
  | Flag of (string * char option)
  | Opt of (string * char option)

type parse_argv_t = {
  name : string;
  args : string list;
  flags : string list;
  opts : (string * string list) list;
}

(* DUPLICATED CODE END*)

(** [starts_with str sub] returns whether [str] starts with [sub].*)
let rec starts_with str sub =
  try String.sub str 0 (String.length sub) = sub
  with Invalid_argument s -> false

(** [drop str n] returns [str] without the first [n] characters. If
    [str] is shorter than [n] characters, the empty string is returned.*)
let drop str n =
  try String.sub str n (String.length str - n) with _ -> ""

(** [prepend_assoc key x] takes an association list where keys are
    mapped to a list. Then, for all keys = [key], [x] is cons-ed onto
    the list.*)
let prepend_assoc key x =
  List.map (fun (k, v) -> (k, if k = key then x :: v else v))

(** [read_lines] returns a list of strings, where each entry is a line
    in [ch] (without the newline character). The list contains an entry
    for each line in [ch] and with the same order. Requies: [ch] is
    readable.*)
let rec read_lines (ch : in_channel) : string list =
  let rec step (acc : string list) =
    try step (input_line ch :: acc) with End_of_file -> acc
  in
  List.rev (step [])

(** [parse_long_arg token rules args acc] attempts to parse [token]
    according to [rules]. If [token] is not a long flag, or is a long
    option missing its parameter, returns an [Error] with a descriptive
    error message. Otherwise, it continues parsing the rest of the
    command line (as specified by [args]), and updates and returns
    [acc]. If any parsing errors are discovered further down the chain,
    [parse_long_arg] will return the first such error instead.

    Requires: the invariant of [args] must hold, that is, [args.opts]
    must contain a key for every option in [rules].*)
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
      else Error ("Unrecognized long flag --" ^ arg)
  | arg :: h :: t ->
      if
        has_rule (function
          | Opt (f, _) when f = arg -> true
          | _ -> false)
      then
        let param = List.fold_left (fun a b -> a ^ "=" ^ b) h t in
        parse_worker rules args
          { acc with opts = prepend_assoc arg param acc.opts }
      else Error ("Unrecognized long option --" ^ arg)

(** [parse_short_arg token rules args acc] attempts to parse [token]
    according to [rules]. If [short] is not a short flag, or is a short
    option missing its parameter, returns an [Error] with a descriptive
    error message. Otherwise, it continues parsing the rest of the
    command line in the same manner as [parse_long_arg].

    Requires: the invariant of [args] must hold, that is, [args.opts]
    must contain a key for every option in [rules].*)
and parse_short_arg token rules args acc =
  if token = "" then parse_worker rules args acc
  else
    let rest = drop token 1 in
    let cur = token.[0] in
    match
      rules
      |> List.find_opt (function
           | Flag (_, Some s) -> s = cur
           | Opt (_, Some s) -> s = cur
           | _ -> false)
    with
    | None -> Error ("Unrecognized short option -" ^ String.make 1 cur)
    | Some (Flag (arg, _)) ->
        parse_short_arg rest rules args
          { acc with flags = arg :: acc.flags }
    | Some (Opt (arg, _)) -> (
        let continue_with_param param remaining_args =
          parse_worker rules remaining_args
            { acc with opts = prepend_assoc arg param acc.opts }
        in
        match args with
        | h :: t -> continue_with_param h t
        | _ when rest <> "" -> continue_with_param rest args
        | [] -> Error ("Short option -" ^ arg ^ " requires parameter") )

(** [parse_worker rules args acc] is the entry point for command line
    parsing. *)
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

let parse_argv (rules : parse_rule_t list) (argv : string array) =
  match Array.to_list argv with
  | [] -> raise (Invalid_argument "argv must have at least one entry")
  | name :: args ->
      (* invariant: [opts] contains a key for each opt rule *)
      let opts =
        rules
        |> List.filter_map (function
             | Opt (key, _) -> Some (key, [])
             | _ -> None)
      in
      parse_worker rules args { name; args = []; flags = []; opts }
