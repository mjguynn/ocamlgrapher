(** Representation of the program's current configuration. *)

(** The abstract type representing the way the program was configured on
    the command line.*)
type t

(** [command_t] represents the "action" that the user wants the program
    to perform on their provided equation. [Graph] means to graph the
    equation. [Points] means to calculate a list of points satisfying
    the equation. [Roots] means to calculate a list of roots of the
    equation. [Extrema] means to calculate a list of extrema of the
    equation. *)
type command_t =
  | Graph
  | Points
  | Roots
  | Extrema

(** [from_cmdline argv ic default_domain default_range] takes an
    argv-style list of args [argv] and uses it to construct a
    [Config.t]. It follows the
    {{:https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}
    GNU Program Argument Syntax Conventions}. It recognizes the
    following options:

    "-g", "--graph": Make [Graph] the command to be executed.

    "-p", "--points": Make [Points] the command to be executed.

    "-r", "--roots": Make [Roots] the command to be executed.

    "-e", "--extrema": Make [Extrema] the command to be executed.

    "--domain-min=[num]": Set minimum value on domain

    "--domain-max=[num]": Set maximum value on domain

    "--range-min=[num]": Set minimum value on range

    "--range-max=[num]": Set maximum value on range

    "-o[file]", "--output=[file]": Write output to [file].

    If "-h" or "--help" is present on the command line, program help
    will be printed to stderr, and then the program will instantly
    terminate with an error code of 0.

    If the number of arguments not paired to an option is exactly one,
    that argument will be the equation. Otherwise, this function shall
    return an error message.

    If one or both components of the domain or range are unspecified on
    the command line, the returned representation shall inherit their
    values from the [default_domain] and [default_range] parameters.

    If no command option is provided on the command line, the returned
    representation will have [Graph] as the command.

    If the commands are invalid in another way (unknown command, etc) an
    error message will be returned.

    Duplicate options are disallowed, and will cause an error message to
    be returned. Conflicting options (-g, -p, -r, -e) will also cause an
    error message to be returned.

    [istream] represents an input stream to read from. Outside of test
    suites, it should probably be stdin.

    [argv] must have at least one entry.*)
val from_cmdline :
  string array ->
  in_channel ->
  float * float ->
  float * float ->
  (t, string) result

(** [equation cfg] returns the unaltered equation string which the user
    supplied to [ocamlgrapher].*)
val equation : t -> string

(** [domain cfg] returns the domain [(a, b)] which constrains all
    commands on this [cfg]. All points [(x,y)] where [x < a] or [x > b]
    should be ignored by the program. It is guaranteed that [b >= a].*)
val domain : t -> float * float

(** [range cfg] returns the range [(c, d)] which all commands on this
    [cfg] should respect. All points [(x,y)] where [y < c] or [y > d]
    should be ignored by the program. It is guaranteed that [d >= c].*)
val range : t -> float * float

(** [command cfg] returns the [command_t] that the user wanted to
    perform. *)
val command : t -> command_t

(** [output_file cfg] returns [Some filepath] if the user specified a
    file [filepath] to write output to. Otherwise, returns [None].*)
val output_file : t -> string option

(** [to_string cfg] returns the human-readable string representation of
    [cfg]. *)
val to_string : t -> string

(** [help errc] prints program help, then exits with a return code of
    [errc]. *)
val help : int -> 'a
