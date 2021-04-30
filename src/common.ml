(** Contains helper functions which are potentially useful to multiple
    modules. *)

(** A [point] is an (x,y) point on the Cartesian XY plane, where x and y
    are finite floats.*)
type point = float * float

(** [points] represents a list of points. *)
type points = point list

(** [starts_with str sub] returns whether [str] starts with [sub]. All
    strings start with the empty string.*)
let rec starts_with str sub =
  try String.sub str 0 (String.length sub) = sub
  with Invalid_argument s -> false

(** [drop str n] returns [str] without the first [n] characters. If
    [str] is shorter than [n] characters, the empty string is returned.*)
let drop str n =
  try String.sub str n (String.length str - n) with _ -> ""

(** [prepend_assoc key x] takes an association list where keys are
    mapped to a list. Then, for all keys = [key], [x] is cons-ed onto
    the list.*)
let prepend_assoc key x =
  List.map (fun (k, v) -> (k, if k = key then x :: v else v))

(** [span (min, max)] is syntactic sugar for [max -. min]. *)
let span (min, max) = max -. min

(** [regular_float f] returns true if [f] is NOT infinite or NaN.*)
let regular_float f =
  classify_float f <> FP_infinite && classify_float f <> FP_nan

(** [valid bounds (a, b)] returns whether the bounds [a..b] are valid,
    where valid bounds are those bounds where [a] and [b] are *finite*
    and [a<=b]. *)
let valid_bounds (a, b) = regular_float a && regular_float b && a <= b

(** [fpeq ?tolerance:t a b] returns whether [a] and [b] are roughly
    equal (within some tolerance for floating-point precision loss). The
    higher the value of [t], the less precise the comparison. By
    default, [t] is [1e3]. (This value was empircally chosen for best
    results.)*)
let fpeq ?tolerance:(t = 1e3) a b =
  let epsilon_scale =
    Float.max 1. (Float.max (Float.abs a) (Float.abs b))
  in
  Float.abs (b -. a) <= Float.epsilon *. epsilon_scale *. t

(** [trunc x] rounds [x] to 0 if it's really close to 0.*)
let trunc x = if fpeq x 1e-13 then 0. else x

(** [avg a b] returns the average of two ints [a] and [b]. *)
let avg a b = (a + b) / 2

(** [flip f] flips the sign of [f].*)
let flip f = f *. -1.0

(** [split pred lst] partitions [lst] into a list of lists by splitting
    on every element for which [pred] is true. The produced list does
    not contain any empty lists. Order is preserved.

    Example:
    [split (( = ) 'a') \['f'; 'a'; 'd'; 'g'; 'a'; 'a'; 'c'\] = \[
    \['f'\]; \['d'; 'g'\]; \['c'\]\]]*)
let split pred =
  (* reject functional, return to ~~monke~~ imperative *)
  let cur = ref [] in
  let prevs = ref [] in
  let rec step = function
    | [] ->
        let lst =
          if !cur <> [] then List.rev !cur :: !prevs else !prevs
        in
        List.rev lst
    | h :: t ->
        if not (pred h) then cur := h :: !cur
        else if !cur <> [] then (
          prevs := List.rev !cur :: !prevs;
          cur := [] );
        step t
  in
  step
