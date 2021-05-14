(** Implementation of module [Numericalmethods].*)

exception Invalid_bounds

exception No_points

let limiter x_b y_b points =
  let open Common in
  if not (valid_bounds x_b && valid_bounds y_b) then
    raise Invalid_bounds
  else
    List.filter
      (fun (px, py) -> in_bounds px x_b && in_bounds py y_b)
      points

(** [diff_signs] returns whether [a] and [b] have different signs. *)
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
      if Common.fpeq y 0. then
        match t with
        | [] -> root_est_help (x, y) (x :: acc) t
        | (x_next, y_next) :: tt ->
            root_est_help (x_next, y_next) (x :: acc) tt
      else if diff_signs ycur y then
        root_est_help (x, y) ((0.5 *. (x +. xcur)) :: acc) t
      else root_est_help (x, y) acc t

let root_estimator lst : float list =
  match lst with
  | [] -> []
  | (x, y) :: t -> t |> root_est_help (x, y) [] |> List.rev

(* helper method to obtain the maximum point of a function output *)
let rec max_help lst (max_x, max_y) (acc : (float * float) list) =
  match lst with
  | [] -> List.rev acc
  | (x, y) :: tail ->
      if Common.fpeq y max_y then
        let lst = (x, y) :: acc in
        max_help tail (x, y) lst
      else if y > max_y then
        let lst = [ (x, y) ] in
        max_help tail (x, y) lst
      else max_help tail (max_x, max_y) acc

let max_output lst =
  match lst with
  | [] -> raise No_points
  | (x, y) :: tail -> max_help lst (0., Float.neg_infinity) []

(* helper method to obtain the minimum point of a function output *)
let rec min_help lst (min_x, min_y) acc =
  match lst with
  | [] -> List.rev acc
  | (x, y) :: tail ->
      if Common.fpeq y min_y then min_help tail (x, y) ((x, y) :: acc)
      else if y < min_y then min_help tail (x, y) [ (x, y) ]
      else min_help tail (min_x, min_y) acc

let min_output lst =
  match lst with
  | [] -> raise No_points
  | (x, y) :: tail -> min_help lst (0., Float.infinity) []

let make_samples (low, high) steps =
  let step_size = Common.span (low, high) /. float_of_int steps in
  let rec do_step cur acc =
    if cur <= low then acc else do_step (cur -. step_size) (cur :: acc)
  in
  do_step high []
