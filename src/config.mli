(** Representation of the program's current command-line configuration. *)

(** The abstract type representing the way the program was configured on
    the command line.*)
type t

(** [from_cmdline default_x_span default_y_span default_quality
    default_output_file ic argv] takes an argv-style list of args [argv]
    and uses it to construct a [Config.t]. It follows the
    {{:https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}
    GNU Program Argument Syntax Conventions}. It recognizes the
    following options:

    "-g", "--graph": Make [Graph] the command to be executed.

    "-p", "--points": Make [Points] the command to be executed.

    "-r", "--roots": Make [Roots] the command to be executed.

    "-e", "--extrema": Make [Extrema] the command to be executed.

    "--x-min=[num]": Set minimum value on X axis

    "--x-max=[num]": Set maximum value on X axis

    "--y-min=[num]": Set minimum value on Y axis

    "--y-max=[num]": Set maximum value on Y axis

    "-q [num]", "--quality=[num]": The number of "steps" in the graph.
    num > 0

    "-o [file]", "--output=[file]": Write output to [file].

    If "-h" or "--help" is present on the command line, program help
    will be printed to stderr, and then the program will instantly
    terminate with an error code of 0.

    There must be at least one argument unpaired to an option; all such
    arguments are treated as equations. If no equations are supplied, an
    error message is returned.

    If one or both components of the X or Y bounds are unspecified on
    the command line, the returned representation shall inherit their
    values from the [default_x_bounds] and [default_y_bounds]
    parameters. Similarly, if the number of steps is unspecified on the
    command line, the returned representation shall use [default_steps]
    steps, and if no output file is specified on the command line, the
    returned representation shall use [default_output_file]. Requires:
    [default_steps] >= 1

    If no command option is provided on the command line, the returned
    representation will have [Graph] as the command.

    Duplicate options are disallowed, and will cause an error message to
    be returned. Conflicting options (-g, -p, -r, -e) will also cause an
    error message to be returned.

    If the commands are invalid in another way (unknown command, etc) an
    error message will be returned.

    [istream] represents an input stream to read from. Outside of test
    suites, it should probably be [stdin].

    [argv] must have at least one entry.*)
val from_cmdline :
  float * float ->
  float * float ->
  int ->
  string ->
  in_channel ->
  string array ->
  (t, string) result

(** [equations cfg] returns the unaltered equation strings that the user
    supplied to [ocamlgrapher], in the order they were originally
    supplied. This list is guaranteed to be non-empty.*)
val equations : t -> string list

(** [x_bounds cfg] returns the bounds on the X axis [(a, b)] which
    constrains all commands on this [cfg]. All points [(x,y)] where
    [x < a] or [x > b] should be ignored by the program. It is
    guaranteed that [b >= a].*)
val x_bounds : t -> float * float

(** [y_bounds cfg] returns the bounds on the Y axis [(c, d)] which all
    commands on this [cfg] should respect. All points [(x,y)] where
    [y < c] or [y > d] should be ignored by the program. It is
    guaranteed that [d >= c].*)
val y_bounds : t -> float * float

(** [steps cfg] returns the number of "steps" or "slices" that the user
    wants the program to use in computing the graph. A higher number of
    slices means more precise output. This value is guaranteed to be at
    least one.*)
val steps : t -> int

(** [command cfg] returns the [Defs.command_t] that the user wanted to
    perform. *)
val command : t -> Defs.command_t

(** [output_file cfg] returns the desired output filename for graphs.*)
val output_file : t -> string

(** [to_string cfg] returns the human-readable string representation of
    [cfg]. *)
val to_string : t -> string

(** [help errc] prints program help, then exits with a return code of
    [errc]. *)
val help : int -> 'a
