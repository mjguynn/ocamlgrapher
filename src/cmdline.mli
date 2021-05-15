(** Parsing of the program command line, using the
    {{:https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}
    GNU Program Argument Syntax Conventions}.*)

(** The abstract type representing a valid, parsed command line.*)
type t

(** [parse_cmdline rules argv] parses the command line [argv] using the
    rules specified in [rules], according to the GNU Program Argument
    Syntax conventions. If an error is encountered, it returs [Error s],
    where s is a descriptive error message; otherwise, it returns
    [Ok v], where v is a valid [t]. [istream] represents the stream that
    should be used for program input. (Unless you're writing unit tests,
    it should probably be stdin.)

    Requires: [argv] must have at least one entry.*)
val parse_cmdline :
  Defs.parse_rule list ->
  in_channel ->
  string array ->
  (t, string) result

(** [name t] returns the name of the binary being executed, based off of
    command line [t].*)
val name : t -> string

(** [args t] returns the list of arguments on command line [t]. An
    "argument" is defined as anything that is not an option and isn't a
    parameter of an option. This list is in right-to-left order with
    respect to how they were inputted.*)
val arguments : t -> string list

(** [flags t] returns the list of all flags passed on command line [t]
    according to the parse rules [t] was constructed with, in
    right-to-left order with respect to how they were originally
    entered. If a flag has a long and short form, the long form will be
    the one present in [flags], regardless of which form appears on the
    command line. For example, if you ran <code>./binary --no-color
    \--confirm</code>, [flags t] would be [\["confirm";"no-color"\]].*)
val flags : t -> string list

(** [opts t] returns an association list where each key represents a
    parameterized option in the parse rules [t] was constructed with.
    EVERY parameterized option in the rules is represented. Each key is
    unique, and if the parameterized option has long and short forms,
    then the long form is the key, regardless of what form(s) appeared
    on the command line. The order of the keys are unspecified. The
    value of each key is a list of all the parameters assigned to the
    key's option on the command line [t] in right-to-left order. If the
    value is an empty list, then the option did not appear on the
    command line. For example, if you ran <code>./binary --open=sesame
    \--debug=5 -o door</code>, with <code>-o</code> as a shortform for
    <code>--open</code>, [opts] could be
    [\[("open", \["door"; "sesame"\]); ("debug", \["5"\])\]] *)
val options : t -> (string * string list) list
