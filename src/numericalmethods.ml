(* These are numerical methods for limiting the range, estimating the
   root and finding the local maximun / minimum within the given domain *)
type t = (float * float) list

exception InvalidRange of string

exception Empty_type of string

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

   RI: list of outputs is always in order: x1 < xn. *)
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
      if y = 0. then
        match t with
        | [] -> root_est_help (x, y) (x :: acc) t
        | (x_next, y_next) :: tt ->
            root_est_help (x_next, y_next) (x :: acc) tt
      else if diff_signs ycur y then
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

let trunc x = if abs_float x < 1e-13 then 0. else x

let tuple_print (x, y) =
  Printf.printf "(%10g, %-10g)\n" (trunc x) (trunc y)

let rec tuple_list_print lst =
  match lst with
  | [] -> print_string "\n"
  | (x, y) :: t ->
      tuple_print (x, y);
      tuple_list_print t

(* helper method to obtain the maximum point of a function output *)
let rec max_help lst (max_x, max_y) (acc : (float * float) list) =
  match lst with
  | [] -> List.rev acc
  | (x, y) :: tail ->
      (*tuple_print (max_x, max_y);*)
      if y > max_y then
        let lst = [ (x, y) ] in
        (*tuple_list_print lst;*)
        max_help tail (x, y) lst
      else if y = max_y then
        let lst = (x, y) :: acc in
        (*tuple_list_print lst;*)
        max_help tail (x, y) lst
      else max_help tail (max_x, max_y) acc

let max_output lst =
  match lst with
  | [] -> failwith "Type is empty"
  | (x, y) :: tail -> max_help lst (0., Float.neg_infinity) []

(* helper method to obtain the minimum point of a function output *)
let rec min_help lst (min_x, min_y) acc =
  match lst with
  | [] -> List.rev acc
  | (x, y) :: tail ->
      if y < min_y then min_help tail (x, y) [ (x, y) ]
      else if y = min_y then min_help tail (x, y) ((x, y) :: acc)
      else min_help tail (min_x, min_y) acc

let min_output lst =
  match lst with
  | [] -> failwith "Type is empty"
  | (x, y) :: tail -> min_help lst (0., Float.infinity) []
