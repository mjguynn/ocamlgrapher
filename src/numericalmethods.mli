(** Methods which analyze and transform lists of points. *)
open Common

(** [Invalid_bounds] means that the bounds passed to a function are
    invalid, such as if you passed (a1, a2) and a1 >= a2.*)
exception Invalid_bounds

(** [No_points] means that a function requiring a list of one or more
    points was provided with an empty list.*)
exception No_points

(** [limiter (x1, x2) (y1, y2) points] returns [points] but without any
    point (x,y) where (x < x1), (x > x2), (y < y1), or (y > y2). Raises
    [Invalid_bounds] if the lower limit is strictly greater than upper
    limit. *)
val limiter : bounds -> bounds -> points -> points

(** [root_estimator fun_output] estimates the roots of the function from
    the list of outputs given by the function. If there are no roots,
    the function returns an empty list. If there are roots, returns the
    approximate x-coordinates of the roots. Order of roots is
    left-to-right.*)
val root_estimator : points -> float list

(** [max_output fun_output] estimates the maximum point(s) of the
    function output from the given list. Output is in left-to-right
    order. Raises [No_points] if there are no input points ([fun_output]
    is the empty list)*)
val max_output : points -> point list

(** [min_output fun_output] estimates the minimum point(s) of the
    function output from the given list. Output is in left-to-right
    order. Raises [No_points] if there are no input points ([fun_output]
    is the empty list)*)
val min_output : points -> point list

(** [make_samples (low, high) steps] generates a list of [steps] values
    evenly distributed between [low] and [high]. Requires: the provided
    bounds are valid*)
val make_samples : bounds -> int -> float list
