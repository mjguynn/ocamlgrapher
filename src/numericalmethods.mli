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
val limiter : float * float -> float * float -> points -> points

(** [limiter_2 (x1, x2) (y1, y2) points] returns a [points] list. Each
    [points] in the points list is separated when a value is outside the
    given domain. Raises [Invalid_bounds] if the lower limit is strictly
    greater than upper limit.

    Here's an example of a valid points list: limiter_2 (-10. , 10.)
    (-10. , 10.)
    [(5., 9.5); (5.5, 9.7); (6., 10.3);(6.5, 9.7);(7., 9.5)] =
    [\[(5., 9.5); (5.5, 9.7)\] ; \[(6.5, 9.7);(7., 9.5)\]] *)
val limiter_2 : float * float -> float * float -> points -> points list

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
val max_output : points -> (float * float) list

(** [min_output fun_output] estimates the minimum point(s) of the
    function output from the given list. Output is in left-to-right
    order. Raises [No_points] if there are no input points ([fun_output]
    is the empty list)*)
val min_output : points -> (float * float) list
