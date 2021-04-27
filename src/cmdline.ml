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
  rules : parse_rule_t list;
}

let rec parse_short_arg token args acc =
  let is_short_rule c =
    acc.rules
    |> List.find_opt (function
         | Flag (_, Some s) -> s = c
         | Opt (_, Some s) -> s = c
         | _ -> false)
  in
  if token = "" then Ok (args, acc)
  else
    let cur, rest = (token.[0], drop token 1) in
    match is_short_rule cur with
    | None -> Error ("Unrecognized short option -" ^ String.make 1 cur)
    | Some (Flag (arg, _)) ->
        parse_short_arg rest args { acc with flags = arg :: acc.flags }
    | Some (Opt (arg, _)) -> (
        let return_with param t =
          Ok (t, { acc with opts = prepend_assoc arg param acc.opts })
        in
        match args with
        | _ when rest <> "" -> return_with rest args
        | h :: t -> return_with h t
        | [] -> Error ("Short option -" ^ arg ^ " requires parameter") )

let parse_long_arg token args acc =
  let is_long_rule s =
    acc.rules
    |> List.find_opt (function
         | Flag (f, _) -> f = s
         | Opt (o, _) -> o = s)
  in
  match String.split_on_char '=' token with
  | [] -> raise (Invalid_argument "Impossible state")
  | arg :: t -> (
      match (is_long_rule arg, t) with
      | Some (Flag (f, _)), _ ->
          Ok (args, { acc with flags = f :: acc.flags })
      | Some (Opt (o, _)), h :: t ->
          let param = List.fold_left (fun a b -> a ^ "=" ^ b) h t in
          Ok (args, { acc with opts = prepend_assoc arg param acc.opts })
      | Some (Opt (o, _)), [] ->
          Error ("Long option --" ^ o ^ " requires a parameter")
      | None, _ -> Error ("Unrecognized long flag or option --" ^ arg) )

(** [parse_worker args acc] is the entry point for command line parsing.
    If successful, it will return an [Ok v], where [v] is a proper value
    of [t]. If unsuccessful, it will return an [Error e], where [e] is a
    descriptive error message.

    Requires: [acc] complies with the invariant, that is, [acc.opts] has
    a key for every parametrized option in [acc.rules].*)
let rec parse_worker (ic : in_channel) (args : string list) (acc : t) =
  match args with
  | [] -> Ok acc
  | "--" :: t -> Ok { acc with args = List.rev_append t acc.args }
  | "-" :: t ->
      parse_worker ic t
        { acc with args = List.append (Io.read_lines ic) acc.args }
  | h :: t ->
      let continue = function
        | Ok (t, acc) -> parse_worker ic t acc
        | Error e -> Error e
      in
      if starts_with h "--" then
        parse_long_arg (drop h 2) t acc |> continue
      else if starts_with h "-" then
        parse_short_arg (drop h 1) t acc |> continue
      else parse_worker ic t { acc with args = h :: acc.args }

let parse_cmdline rules ic argv =
  match Array.to_list argv with
  | [] -> raise (Invalid_argument "argv must have at least one entry")
  | name :: args ->
      (* invariant: [opts] contains a key for each opt rule *)
      let opts =
        List.filter_map
          (function Opt (k, _) -> Some (k, []) | _ -> None)
          rules
      in
      parse_worker ic args { name; args = []; flags = []; opts; rules }

let name t = t.name

let arguments t = t.args

let flags t = t.flags

let options t = t.opts
