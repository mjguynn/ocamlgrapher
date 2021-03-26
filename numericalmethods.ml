(* These are numerical methods for limiting the range, estimating the
   root and finding the local maximun / minimum within the given domain *)
type t = (float * float) list

(* Requires that the range is valid. Valid Range is a float tuple of
   format (ymin, ymax), and ymin <= ymax *)

let rec range_lim_help (ymin, ymax) acc output_list =
  match output_list with
  | [] -> acc
  | (x, y) :: t ->
      if y < ymax && y > ymin then
        range_lim_help (ymin, ymax) ((x, y) :: acc) t
      else range_lim_help (ymin, ymax) acc t

(* AF: [(x1, y1); (x2,y2); ... (xn, yn)] is the list of the outputs of
   an explicit, continuous function f given by user.

   RI: list of outputs is always in order: x1 < xn.

   Time Complexity: helper function is O(n), list reverser is O(n), so
   the total time complexity is O(n^2) *)
let range_limiter (output_list : t) range : t =
  match range with
  | ymin, ymax ->
      if ymin <= ymax then
        output_list |> range_lim_help (ymin, ymax) [] |> List.rev
      else failwith "Invalid Range: ymin > ymax"

(* checks if the signs flipped going from a to b *)
let diff_signs (a : float) (b : float) : bool =
  let pos_to_neg = a >= 0. && b <= 0. in
  let neg_to_pos = a <= 0. && b >= 0. in
  if pos_to_neg || neg_to_pos then true else false

(* helper to estimate the roots *)
let rec root_est_help output_list (xcur, ycur) acc =
  match output_list with
  | [] -> acc
  | (x, y) :: t ->
      if diff_signs ycur y then
        root_est_help t (x, y) ((0.5 *. (x +. xcur)) :: acc)
      else root_est_help t (x, y) acc

(* AF: the float list [x1; x2; x3 ... xk] represents the set of
   estimated roots from the output list of the given fuction *)
let root_estimator (output_list : t) : float list =
  match output_list with
  | [] -> []
  | (x, y) :: t -> root_est_help t (x, y) []
