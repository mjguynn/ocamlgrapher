(** Implementation of moduke [Svghelpers].*)
open Xmldom

let make_text c atts x y txt =
  Container
    ("text", atts @ [ ("class", c); ("x", x); ("y", y) ], [ Text txt ])

let make_circle c atts x y r =
  Item
    ("circle", atts @ [ ("class", c); ("cx", x); ("cy", y); ("r", r) ])

let make_polyline c atts points =
  let coords =
    List.fold_left
      (fun acc (x, y) -> Printf.sprintf "%s %f,%f" acc x y)
      "" points
  in
  Item ("polyline", atts @ [ ("class", c); ("points", coords) ])

let make_region_border c atts (x1, y1) (x2, y2) =
  Container
    ( "g",
      atts,
      [
        (* left *)
        make_polyline c [] [ (x1, y1); (x1, y2) ];
        (* right *)
        make_polyline c [] [ (x2, y1); (x2, y2) ];
        (* top *)
        make_polyline c [] [ (x1, y1); (x2, y1) ];
        (* bottom *)
        make_polyline c [] [ (x1, y2); (x2, y2) ];
      ] )
