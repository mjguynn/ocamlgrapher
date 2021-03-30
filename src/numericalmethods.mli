(* Testing GitHub Desktop *)

type t = (float * float) list

exception InvalidRange of string

exception EmptyList of string

(** converts prototype list to type t. RI: the list MUST be of
    (float*float) list *)
val get_t : (float * float) list -> t

(** [range_limiter fun_output range] cuts off any values of y within
    range. The type of range is (float * float), and the lower limit
    must come first. Raises exception InvalidRange if the lower limit is
    strictly greater than upper limit. A function output is validly
    within range if the value is greater than/less than or equal to the
    lower/upper limit. *)
val range_limiter : t -> float * float -> t

(** [root_estimator fun_output] estimates the roots of the function from
    the list of outputs given by the function. If there are no roots,
    the function returns an empty list. If there are roots, returns the
    approximate x-coordinates of the roots.*)
val root_estimator : t -> float list

(** [max_output fun_output] estimates the RIGHT-MOST maximum of the
    function output from the given list. Returns the output. RI: cannot
    pass an empty type t *)
val max_output : t -> float * float

(** [min_output fun_output] estimates the RIGHT-MOST minimum of the
    function output from the given list. Returns the output. RI: cannot
    pass an empty type t *)
val min_output : t -> float * float
