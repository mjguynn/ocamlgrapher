(** Parsing of the program command line, using the GNU
    {{:https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}
    GNU Program Argument Syntax Conventions}.*)

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

val parse_argv :
  parse_rule_t list -> string array -> (parse_argv_t, string) result
