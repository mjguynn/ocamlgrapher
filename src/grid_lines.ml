let abs_floor (num : float) : float =
  if num > 0. then floor num else ceil num

let compute_error (increment : float) : float =
  abs_float (Float.round increment -. increment)

let y_max_line_count = 10

let y_axis_endpoint = 10.

(* Fixed-ish number of grid lines for range, number of grid-lines for
   domain depends on what multiple domain's range is of range's range *)

(* Discard if increment 0 or greater than range *)
let compute_increment (range : float) (line_count : int) : float =
  (* possible increment of multiple of 10^n *)
  let increment_1 =
    log10 (Float.trunc (range /. float_of_int line_count))
  in
  (* possible increment of multiple of 2(10^n) *)
  let increment_2 = increment_1 -. log10 2. in
  (* possible increment of multiple of 5(10^n) *)
  let increment_5 = increment_1 -. log10 5. in
  let error_1 = compute_error increment_1 in
  let error_2 = compute_error increment_2 in
  let error_5 = compute_error increment_5 in
  if error_1 < error_2 && error_1 < error_5 then
    10. ** Float.round increment_1
  else if error_2 < error_1 && error_2 < error_5 then
    2. *. (10. ** Float.round increment_2)
  else 5. *. (10. ** Float.round increment_5)

let rec select_increment range line_count =
  let increment = compute_increment range line_count in
  match line_count with
  | 2 -> (2, increment)
  | n ->
      if increment < range then (n, increment)
      else select_increment range (n - 1)

let rec increment_to_coords acc increment =
  match acc with
  | h :: t ->
      if h -. increment <= 0. then h :: t
      else increment_to_coords ((h -. increment) :: h :: t) increment
  | _ -> failwith "Invalid list supplied"

let get_grid_pos
    (x_min : float)
    (x_max : float)
    (y_min : float)
    (y_max : float) =
  let x_range = abs_floor x_max -. abs_floor x_min in
  let y_range = abs_floor y_max -. abs_floor y_min in
  let x_range_scale = x_range /. y_range in
  let x_axis_endpoint = y_axis_endpoint *. x_range_scale in
  let x_max_line_count =
    int_of_float
      (Float.round (x_range_scale *. float_of_int y_max_line_count))
  in
  let y_increment = select_increment y_range y_max_line_count in
  let x_increment = select_increment x_range x_max_line_count in
  ( begin
      match x_increment with
      | n, k ->
          increment_to_coords [ x_axis_endpoint ]
            (x_axis_endpoint /. float_of_int n)
    end,
    match y_increment with
    | n, h ->
        increment_to_coords [ y_axis_endpoint ]
          (y_axis_endpoint /. float_of_int n) )
