(** A graphical representation of equations. *)

(** [Grapher.t] is the abstract representation of a graph.*)
type t

(** [create (x_min, x_max) (y_min, y_max) aspect_ratio] creates a new,
    empty graph that views the area [(x_min, x_max)] X [(y_min, y_max)].
    The aspect ratio of a square unit on the graph is determined by
    [aspect_ratio].

    Requires: [x_min < x_max], [y_min < y_max], and
    [0 < aspect_ratio < +inf]. *)
val create : float * float -> float * float -> float -> t

(** [add_plot label segments graph] adds [segments] to [graph], where
    each segment is a list of points, and each point within a segment is
    connected to the next point within that segment (if one exists) via
    a straight line. [label] is the label of the relation on the
    resulting graph. Example usage:
    [add_equation "y=x" \[(0,0); (1,1); (2,2)\] g] *)
val add_plot : string -> Common.points list -> t -> t

(** [to_svg filename graph] outputs an SVG representing [graph] to
    [filename]. [filename] represents the name of the output SVG file,
    including extension. If any errors occur (for example, write access
    denied) an unspecified exception will be thrown.*)
val to_svg : string -> t -> unit
