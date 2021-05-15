(** Implementation of module [Cmdline].*)

open Common
open Defs

(** AF: Represents a valid, parsed command line where the binary name is
    [name].

    The list of free arguments, in the reverse order that they appeared
    on the command line, is [args].

    The list [flags] is the list of all flags that appeared on the
    command line, including duplicates, using their longform names
    exclusively, and in the reverse order that they appeared on the
    command line.

    The list [opts] contains a key for every longform option in the
    rules the [t] was built with, with the values for each option being
    a list of all the parameters assigned to that option in the reverse
    order they appeared on the command line.

    The list [rules] is the full list of rules used to build this [t].
    It's meant for internal use.*)
type t = {
  name : string;
  args : string list;
  flags : string list;
  opts : (string * string list) list;
  rules : parse_rule list;
}

(** [is_short_rule c rules] returns whether [c] is the short-form
    character corresponding to a flag or option in [rules]. For example,
    if [rules] contains
    [Defs.Opt ("testing", Some 't'); Defs.Flag ("flag", None)] and [c]
    is ['t'], [is_short_rule] would return true; if [c] was ['f'],
    [is_short_rule] would return false. *)
let is_short_rule c =
  List.find_opt (function
    | Flag (_, Some s) -> s = c
    | Opt (_, Some s) -> s = c
    | _ -> false)

(** [parse_short_arg token args acc] takes the current command line
    state [acc] and attempts to update it using the short option(s)
    and/or flag(s) in [token]. Suppose [token] refers to "-tf", "-gtf",
    "-f", etc. where ['t'] and ['g'] are shortform for flags and ['f']
    is the shorthand form of an option; then the next element in [args]
    will be consumed as the parameter for that option. Also, suppose
    that [token] refers to "-tf" where ['t'] is shortform for an option;
    then "f" will be interpreted as the parameter for that option. If
    successful, returns a [Ok (new_args, new_acc)] where [new_args] are
    the non-consumed elements of [args] and [new_acc] is the updated
    version of [acc]; on failure, returns an [Error e] with a
    descriptive error message.*)
let rec parse_short_arg token args acc =
  (*token must have at least one character because "-" is a special case
    in parse_worker (reads from input channel) *)
  let cur, rest = (token.[0], drop token 1) in
  match is_short_rule cur acc.rules with
  | None -> Error ("Unrecognized short option -" ^ String.make 1 cur)
  | Some (Flag (arg, _)) ->
      let new_acc = { acc with flags = arg :: acc.flags } in
      if rest <> "" then parse_short_arg rest args new_acc
      else Ok (args, new_acc)
  | Some (Opt (arg, _)) -> (
      let return_option opt_name lst =
        Ok (lst, { acc with opts = prepend_assoc arg opt_name acc.opts })
      in
      match args with
      | _ when rest <> "" -> return_option rest args
      | h :: t -> return_option h t
      | [] -> Error ("Short option -" ^ arg ^ " requires parameter") )

(** [is_long_rule str rules] returns whether [str] is the long-form name
    of a flag or option in [rules]. For example, if [rules] contains
    [Defs.Opt ("testing", Some 't'); Defs.Flag ("flag", None)] and [str]
    is ["testing"] or ["flag"], [is_long_rule] would return true; if
    [str] was ["hello"] or ["t"], [is_long_rule] would return false. *)
let is_long_rule str =
  List.find_opt (function
    | Flag (f, _) -> f = str
    | Opt (o, _) -> o = str)

(** [parse_long_arg token args acc] takes the current command-line state
    [acc] and attempts to update it using the long flag or option
    specified by [parse_long_arg]. Suppose that [token] begins with
    string [a], which contains no equal signs; then [a] may be the
    longform of a flag in [acc], in which case [acc] shall be updated to
    respect its presence. Alternatively, if [a] is the longform of an
    option in [acc], and the string is of the form "a=b" where [b] is
    another string, then [acc] is updated to reflect the option
    specified by [a] recieving the parameter [b]. On success, returns
    [Ok (args, new_acc)] where [new_acc] is the updated state; on
    failure, return [Error e], where [e] is a descriptive error message.
    Note that [args] is not used by [parse_long_arg] except in its
    successful return value.*)
let parse_long_arg token args acc =
  match String.split_on_char '=' token with
  | [] -> ( raise (Invalid_argument "Impossible") [@coverage off] )
  | arg :: t -> (
      match (is_long_rule arg acc.rules, t) with
      | Some (Flag (f, _)), _ ->
          Ok (args, { acc with flags = f :: acc.flags })
      | Some (Opt (o, _)), h :: t ->
          let param = List.fold_left (fun a b -> a ^ "=" ^ b) h t in
          Ok (args, { acc with opts = prepend_assoc arg param acc.opts })
      | Some (Opt (o, _)), [] ->
          Error ("Long option --" ^ o ^ " requires a parameter")
      | None, _ -> Error ("Unrecognized long flag or option --" ^ arg) )

(** [parse_worker input_channel args acc] is the entry point for command
    line parsing. If successful, it will return an [Ok v], where [v] is
    a proper value of [t]. If unsuccessful, it will return an [Error e],
    where [e] is a descriptive error message. If it encounters - on the
    command line, it will read newline-delimited arguments from
    [input_channel].

    Requires: [acc] complies with the invariant, that is, [acc.opts] has
    a key for every parametrized option in [acc.rules].*)
let rec parse_worker (ic : in_channel) (args : string list) (acc : t) =
  let continue = function
    | Ok (t, acc) -> parse_worker ic t acc
    | Error e -> Error e
  in
  match args with
  | [] -> Ok acc
  | "--" :: t -> Ok { acc with args = List.rev_append t acc.args }
  | "-" :: t ->
      parse_worker ic t
        { acc with args = List.append (Io.read_lines ic) acc.args }
  | h :: t ->
      if starts_with h "--" then
        parse_long_arg (drop h 2) t acc |> continue
      else if starts_with h "-" then
        parse_short_arg (drop h 1) t acc |> continue
      else parse_worker ic t { acc with args = h :: acc.args }

let parse_cmdline rules input_channel argv =
  match Array.to_list argv with
  | [] -> raise (Invalid_argument "argv must have at least one entry")
  | name :: args ->
      (* invariant: [opts] contains a key for each opt rule *)
      let opts =
        List.filter_map
          (function Opt (k, _) -> Some (k, []) | _ -> None)
          rules
      in
      parse_worker input_channel args
        { name; args = []; flags = []; opts; rules }

let name t = t.name

let arguments t = t.args

let flags t = t.flags

let options t = t.opts
