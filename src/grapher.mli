(** A graphical representation of equations. *)

(** [Grapher.t] is the abstract representation of a graph.*)
type t

(** [create (domain_min, domain_max) (range_min, range_max)] creates a
    new, empty graph that viewing the area [domain_min, domain_max] X
    [range_min, range_max].

    Requires: [domain_min <= domain_max] and [range_min <= range_max].*)
val create : float * float -> float * float -> t

(** [add_plot label segments graph] adds [segments] to [graph], where
    each segment is a list of points, and each point within a segment is
    connected to the next point within that segment (if one exists) via
    a straight line. [label] is the label of the relation on the
    resulting graph. Example usage:
    [add_equation "y=x" \[(0,0); (1,1); (2,2)\] g] *)
val add_plot : string -> (float * float) list -> t -> t

(** [to_svg filename graph] outputs an SVG representing [graph] to
    [filename]. [filename] represents the name of the output SVG file,
    *including extension*. If any errors occur (for example, write
    access denied) an unspecified exception will be thrown.*)
val to_svg : string -> t -> unit
