(** A graphical representation of equations. *)

(** [Grapher.t] is the abstract representation of a graph.*)
type t

(** [create (domain_min, domain_max) (range_min, range_max)] creates a
    new, empty graph that viewing the area [domain_min, domain_max] X
    [range_min, range_max]. *)
val create : float * float -> float * float -> t

(** [add_plot label points graph] adds [points] to [graph], where each
    point in [points] is connected to the next point via a straight
    line. [label] is the label of the line on the resulting
    graph.contents Example usage:
    [add_equation "y=x" \[(0,0); (1,1); (2,2)\] g] *)
val add_plot : string -> (float * float) list -> t -> t

(** [to_svg filename graph] outputs an SVG representing [graph] to
    [filename]. [filename] represents the name of the output SVG file,
    *including extension*. If any errors occur (for example, write
    access denied) a Failure exception will be thrown with a descriptive
    error message.*)
val to_svg : string -> t -> unit
