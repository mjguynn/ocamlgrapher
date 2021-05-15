(** Representation of the program's current command-line configuration. *)

(** The abstract type representing the way the program was configured on
    the command line.*)
type t

(** [from_cmdline default_x_bounds default_y_bounds default_quality
    default_output_file input_channel argv] takes an argv-style list of
    args [argv] and uses it to construct a [Config.t]. It follows the
    {{:https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}
    GNU Program Argument Syntax Conventions}. To identify the flags,
    options, and arguments it recognizes, consult the output of the
    [help] function.

    On success, returns [Ok c], where [c] is the program configuration
    which corresponds to the command line. On failure, returns
    [Error e], where [e] is a descriptive error message.

    [default_x_bounds], [default_y_bounds], [default_quality], and
    [default_output_file] are subsituted if the user did not specify
    them on the command line.

    [input_channel] represents an input stream to read from. Outside of
    test suites, it should probably be [stdin].

    [argv] expects to have at least one entry, the first entry being the
    program itself as executed on the command line (i.e.
    ["./ocamlgrapher"])*)
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

(** [ratio cfg] returns the user-specified aspect ratio for a single
    square unit of the output graph. This value is guaranteed to be
    finite and > 0.*)
val ratio : t -> float

(** [command cfg] returns the [Defs.command_t] that the user wanted to
    perform. *)
val command : t -> Defs.command

(** [output_file cfg] returns the desired output filename for graphs.*)
val output_file : t -> string

(** [to_string cfg] returns the human-readable string representation of
    [cfg]. *)
val to_string : t -> string

(** [help errc] prints program help, then exits with a return code of
    [errc]. *)
val help : int -> 'a
