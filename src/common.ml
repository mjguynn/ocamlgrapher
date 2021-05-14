(** Contains helper functions which are potentially useful to multiple
    modules. *)

(** A [point] is an (x,y) point on the Cartesian XY plane, where x and y
    are finite floats.*)
type point = float * float

(** A [bounds] (a,b) represents the numeric bound \[a..b\], where [a]
    and [b] are finite. *)
type bounds = float * float

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
let span (min, max) = max -. min [@@coverage off]

(** [regular_float f] returns true if [f] is NOT infinite or NaN.*)
let regular_float f =
  classify_float f <> FP_infinite && classify_float f <> FP_nan

(** [valid_bounds (a, b)] returns whether the bounds [a..b] are valid,
    where valid bounds are those bounds where [a] and [b] are *finite*
    and [a<=b]. *)
let valid_bounds (a, b) = regular_float a && regular_float b && a <= b

(** [in_bounds v (a, b)] returns whether [v] is contained within the
    bounds [a..b]. Requires: [(a, b)] is valid according to
    [valid_bounds].*)
let in_bounds v (a, b) =
  assert (valid_bounds (a, b));
  a <= v && v <= b

(** [fpeq ?tolerance:t a b] returns whether [a] and [b] are roughly
    equal (within some tolerance for floating-point precision loss). The
    higher the value of [t], the less precise the comparison. By
    default, [t] is [1e3]. (This value was empircally chosen for best
    results.)*)
let fpeq ?tolerance:(t = 1e3) a b =
  let epsilon_scale =
    Float.max 1. (Float.max (Float.abs a) (Float.abs b))
  in
  Float.abs (b -. a) < Float.epsilon *. epsilon_scale *. t

(** [trunc x] rounds [x] to 0 if it's really close to 0.*)
let trunc x = if fpeq x 1e-13 then 0. else x

(** [hd_opt lst] is like [List.hd], but returns [Some h] if [lst] begins
    with an element [h] and [None] if [lst] is empty.*)
let hd_opt = function h :: _ -> Some h | [] -> None

(** [split pred lst] partitions [lst] into a list of lists by splitting
    on every element [e] for which [pred p e n] is true, where [p] is an
    option containing the element before [e] (or [None] if none exists)
    and [n] is an option containing the element after [e] (or [None] if
    none exists). The produced list does not contain any empty lists.
    Order is preserved and elements are guaranteed to be processed in
    the order they appear in [lst].

    Example:
    [split (fun _ c _ -> c ='a') \['f'; 'a'; 'd'; 'g'; 'a'; 'a'; 'c'\] =
    \[ \['f'\]; \['d'; 'g'\]; \['c'\]\]]*)
let split pred =
  let working = ref [] in
  let processed = ref [] in
  let update prev cur next =
    if not (pred prev cur next) then working := cur :: !working
    else if !working <> [] then (
      processed := List.rev !working :: !processed;
      working := [] )
  in
  let rec step prev = function
    | [] ->
        List.rev
          ( if !working <> [] then List.rev !working :: !processed
          else !processed )
    | cur :: t ->
        update prev cur (hd_opt t);
        step (Some cur) t
  in
  step None

(** [point_oob x_b y_b opt] returns false if [opt = Some (x,y)]
    where[(x,y)] is within the X bounds [x_b] and Y bounds [x_b].
    Otherwise, it returns true.*)
let point_oob x_b y_b = function
  | Some (x, y) -> not (in_bounds x x_b && in_bounds y y_b)
  | None -> true
