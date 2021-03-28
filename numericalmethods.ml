(* These are numerical methods for limiting the range, estimating the
   root and finding the local maximun / minimum within the given domain *)
type t = (float * float) list

exception InvalidRange of string

let get_t (lst : (float * float) list) : t = lst

(* Requires that the range is valid. Valid Range is a float tuple of
   format (ymin, ymax), and ymin <= ymax *)
let rec range_lim_help (ymin, ymax) acc fun_output =
  match fun_output with
  | [] -> acc
  | (x, y) :: t ->
      if y <= ymax && y >= ymin then
        range_lim_help (ymin, ymax) ((x, y) :: acc) t
      else range_lim_help (ymin, ymax) acc t

(* AF: [(x1, y1); (x2,y2); ... (xn, yn)] is the list of the outputs of
   an explicit, continuous function f given by user.

   RI: list of outputs is always in order: x1 < xn.

   Time Complexity: helper function is O(n), list reverser is O(n), so
   the total time complexity is O(n^2) *)
let range_limiter fun_output range : t =
  match range with
  | ymin, ymax ->
      if ymin <= ymax then
        fun_output |> range_lim_help (ymin, ymax) [] |> List.rev
      else raise (InvalidRange "Invalid Range: ymin > ymax")

(* checks if the signs flipped going from a to b *)
let diff_signs (a : float) (b : float) : bool =
  let pos_to_neg = a >= 0. && b <= 0. in
  let neg_to_pos = a <= 0. && b >= 0. in
  if pos_to_neg || neg_to_pos then true else false

(* helper to estimate the roots *)
let rec root_est_help
    (xcur, ycur)
    acc
    (output_list : (float * float) list) =
  match output_list with
  | [] -> acc
  | (x, y) :: t ->
      if diff_signs ycur y then
        root_est_help (x, y) ((0.5 *. (x +. xcur)) :: acc) t
      else root_est_help (x, y) acc t

(* AF: the float list [x1; x2; x3 ... xk] represents the set of
   estimated roots {x1, x2, ... , xk} from the output list of the given
   fuction.

   RI: The set of lists is either empty or non-empty. *)
let root_estimator (output_list : t) : float list =
  match output_list with
  | [] -> []
  | (x, y) :: t -> t |> root_est_help (x, y) [] |> List.rev
