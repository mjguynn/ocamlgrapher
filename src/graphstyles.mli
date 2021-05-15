(** Handles functionality related to the stylesheet of a graph. *)

(** The abstract type representing a graph stylesheet and its embedded
    variables. *)
type t

(** [load filename] attempts to initialize the abstract type from the
    stylesheet in [filename]. If the file does not exist, or was missing
    some required embedded variables, this will return an [Error e]
    where [e] is a descriptive error message; otherwise, it will return
    an [Ok s] where [s] is the resulting stylesheet. The required
    embedded variables are: ["--header-height"], ["--body-min-width"],
    ["--body-min-height"], ["--label-vertical-spacing"],
    ["--label-indent"], ["--label-font-size"]. All embedded variables
    must be defined in the root section of the CSS file and have their
    units specified *in pixels*. *)
val load : string -> (t, string) result

(** [raw_stylesheet s] returns the raw contents of the stylesheet [s] as
    they appeared in the file.*)
val raw_stylesheet : t -> string

(** [header_height t] returns the height of the header section in the
    equation box, as specified in the stylesheet. *)
val header_height : t -> int

(** [body_min_width t] returns the minimum width of the equation box, as
    specified in the stylesheet. *)
val body_min_width : t -> int

(** [body_min_height t] returns the minimum height of the equation box
    (minus the header), as specified in the stylesheet. *)
val body_min_height : t -> int

(** [label_vertical_spacing t] returns the vertical spacing between each
    equation label, as specified in the stylesheet. *)
val label_vertical_spacing : t -> int

(** [label_vertical_spacing t] returns the horizontal indent of the
    equation labels, as specified in the stylesheet. This is the indent
    of the labels themselves, not the colored circles next to them. *)
val label_indent : t -> int

(** [label_font_size t] returns the font size of the equation labels, as
    specified in the stylesheet. *)
val label_font_size : t -> int
