let abs_floor (num : float) : float =
  if num > 0. then floor num else ceil num

let compute_error (increment : float) : float =
  abs_float (Float.round increment -. increment)

(* Fixed-ish number of grid lines for range, number of grid-lines for
   domain depends on what multiple domain's range is of range's range *)
let compute_increment (min : float) (max : float) (line_count : int) :
    float =
  let min_line = abs_floor min in
  let max_line = abs_floor max in
  let range = max_line -. min_line in
  let increment_1 =
    log10 (Float.trunc (range /. float_of_int line_count))
  in
  let increment_2 = increment_1 -. log10 2. in
  let increment_5 = increment_1 -. log10 5. in
  let error_1 = compute_error increment_1 in
  let error_2 = compute_error increment_2 in
  let error_5 = compute_error increment_5 in
  if error_1 < error_2 && error_1 < error_5 then
    10. ** Float.round increment_1
  else if error_2 < error_1 && error_2 < error_5 then
    2. *. (10. ** Float.round increment_2)
  else 5. *. (10. ** Float.round increment_5)
