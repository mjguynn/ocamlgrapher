(** Implementation of module [Cmdline].*)

(* Unfortunately, I have to duplicate some code from the interface. *)
type parse_rule_t =
  | Flag of (string * char option)
  | Opt of (string * char option)

(** AF: Represents a valid, parsed command line where the binary name is
    [name].

    The list of free arguments is [args].

    The list [flags] is the list of all flags that appeared on the
    command line, including duplicates, using their longform names
    exclusively, and in the reverse order that they appeared on the
    command line.

    The list [opts] contains a key for every longform option in the
    rules the [t] was built with, with the values for each option being
    a list of all the parameters assigned to that option in the reverse
    order they appeared on the command line. *)
type t = {
  name : string;
  args : string list;
  flags : string list;
  opts : (string * string list) list;
}

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

(** [parse_worker rules args acc] is the entry point for command line
    parsing. If successful, it will return an [Ok v], where [v] is a
    proper value of [t]. If unsuccessful, it will return an [Error e],
    where [e] is a descriptive error message.

    Requires: [acc] complies with the invariant, that is, [acc.opts] has
    a key for every parametrized option in [rules].*)
let rec parse_worker
    (rules : parse_rule_t list)
    (args : string list)
    (acc : t) =
  let rec parse_short_arg token args acc =
    if token = "" then parse_worker rules args acc
    else
      let cur, rest = (token.[0], drop token 1) in
      match
        rules
        |> List.find_opt (function
             | Flag (_, Some s) -> s = cur
             | Opt (_, Some s) -> s = cur
             | _ -> false)
      with
      | None -> Error ("Unrecognized short option -" ^ String.make 1 cur)
      | Some (Flag (arg, _)) ->
          parse_short_arg rest args
            { acc with flags = arg :: acc.flags }
      | Some (Opt (arg, _)) -> (
          let continue_with_param param remaining_args =
            parse_worker rules remaining_args
              { acc with opts = prepend_assoc arg param acc.opts }
          in
          match args with
          | h :: t -> continue_with_param h t
          | _ when rest <> "" -> continue_with_param rest args
          | [] -> Error ("Short option -" ^ arg ^ " requires parameter")
          )
  in
  let parse_long_arg token args acc =
    let has_rule f = List.exists f rules in
    match String.split_on_char '=' token with
    | [] -> raise (Invalid_argument "Impossible state")
    | [ arg ] ->
        if
          has_rule (function
            | Flag (f, _) when f = arg -> true
            | _ -> false)
        then
          parse_worker rules args { acc with flags = arg :: acc.flags }
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
  in
  match args with
  | "--" :: t -> Ok { acc with args = List.rev_append t acc.args }
  | "-" :: t ->
      let new_acc =
        { acc with args = List.append (read_lines stdin) acc.args }
      in
      parse_worker rules t new_acc
  | h :: t ->
      if starts_with h "--" then parse_long_arg (drop h 2) t acc
      else if starts_with h "-" then parse_short_arg (drop h 1) t acc
      else parse_worker rules t { acc with args = h :: acc.args }
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

let name t = t.name

let arguments t = t.args

let flags t = t.flags

let options t = t.opts
