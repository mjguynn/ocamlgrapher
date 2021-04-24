(** Implementation of module [Grapher].*)

open Xmldom
open Common

(** [hsv (h, s, v)] has hue [h], saturation [s], and value [v]. Each
    value is a normalized float [0..1] *)
type hsv = float * float * float

(** [hsl_string_of_hsv hsv] creates a hue-saturation-lightness string
    (according to the CSS3 specification) corresponding to the HSV color
    [hsv]*)
let hsl_string_of_hsv (h, sv, v) =
  (* This formula was taken from Wikipedia and translated into OCaml *)
  let l = v *. (1. -. (sv /. 2.)) in
  let sl =
    if l = 0. || l = 1. then 0. else (v -. l) /. Float.min l (1. -. l)
  in
  Printf.sprintf "hsl(%f, %f%%, %f%%)" (h *. 360.) (sl *. 100.)
    (l *. 100.)

(** [hsv_step hsv] is a state machine which takes [hsv] and outputs a
    new color. Any color outputted will never match an earlier color in
    the output sequence, to the extent permitted by floating-point
    precision loss. *)
let hsv_step (h, s, v) =
  let step = 0.312 in
  (* PATTERN: hue changes first, then saturation, and finally value *)
  let h' = h +. step in
  if h' > 1. then
    let h'' = h' -. 1. in
    let s' = s -. step in
    if s' < 0. then (h'', 1., v /. 2.) else (h'', s', v)
  else (h', s, v)

(** A [relation] contains information about a relation processed by
    OCamlgrapher. [label] is a human-readable identifier for the
    relation, such as its formula. [points] is a list of selected points
    satisfying the relation, in an order such that drawing a line from
    one point to the next is a reasonable approximation of the relation.
    [color] is the HSV color assigned to this relation on the graph. *)
type relation = {
  label : string;
  points : (float * float) list;
  color : hsv;
}

(** AF: Let [(x1, x2) = x_bounds] and [(y1, y2) = y_bounds]. Then an
    instance of type [t] represents a graph of the relations [relations]
    on a window spanning [x1..x2] on the X-axis and [y1..y2] on the
    Y-axis.

    RI: [x2 > x1] and [y2 > y1]. *)
type t = {
  relations : relation list;
  x_bounds : float * float;
  y_bounds : float * float;
}

let create x_bounds y_bounds = { relations = []; x_bounds; y_bounds }

let add_plot label points g =
  let color =
    match g.relations with
    | [] -> (0., 1., 1.) (* initial color - bright red *)
    | h :: _ -> hsv_step h.color
  in
  { g with relations = { label; points; color } :: g.relations }

let create_text ?fill:(f = "black") c x y txt =
  Container
    ( "text",
      [ ("fill", f); ("class", c); ("x", x); ("y", y) ],
      [ Text txt ] )

let create_line ?fill:(f = "black") c x1 x2 y1 y2 =
  Item
    ( "line",
      [
        ("fill", f);
        ("class", c);
        ("x1", x1);
        ("x2", x2);
        ("y1", y1);
        ("y2", y2);
      ] )

let create_circle ?fill:(f = "black") c x y r =
  Item
    ( "circle",
      [ ("fill", f); ("class", c); ("cx", x); ("cy", y); ("r", r) ] )

let relation_label num eq =
  let col = hsl_string_of_hsv eq.color in
  let h = (num * 30) + 90 in
  Container
    ( "g",
      [],
      [
        create_circle "relation_view_disc" ~fill:col "40"
          (string_of_int h) "10";
        create_text "relation_view_text" ~fill:col "60px"
          (string_of_int (h + 5))
          eq.label;
      ] )

let relations_view g =
  let labels = List.mapi relation_label (List.rev g.relations) in
  let background =
    Item ("rect", [ ("class", "relation_view_background") ])
  in
  let border =
    create_line "relation_view_line" "100%" "100%" "0" "100%"
  in
  let header =
    create_text "relation_view_header" "15px" "40px" "Relations"
  in
  let divider =
    create_line "relation_view_line" "0%" "100%" "60px" "60px"
  in
  let max_label_characters =
    List.fold_left
      (fun maxlen eq -> max maxlen (String.length eq.label))
      0 g.relations
  in
  let width = 60 + max 160 (12 * max_label_characters) in
  let height = 90 + max 160 (30 * List.length g.relations) in
  Container
    ( "svg",
      [ ("viewBox", Printf.sprintf "0 0 %i %i" width height) ],
      background :: border :: header :: divider :: labels )

let plot_view g =
  Container ("svg", [ ("viewBox", "300 0 1000 1000") ], [])

let to_svg filename g =
  (* load stylesheet, create DOM element *)
  let styles = open_in "graph_styles.css" in
  let styles_str =
    read_lines styles |> List.fold_left (fun acc s -> s ^ "\n" ^ acc) ""
  in
  let styles_elem = Container ("style", [], [ Text styles_str ]) in
  close_in styles;
  (* begin export *)
  let f = open_out filename in
  let dom =
    Container
      ( "svg",
        [ ("xmlns", "http://www.w3.org/2000/svg") ],
        [ styles_elem; relations_view g ] )
  in
  output_xml f dom;
  close_out f
