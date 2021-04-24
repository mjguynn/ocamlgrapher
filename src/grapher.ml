open Xmldom
open Common

(** [hsv (h, s, v)] has hue [h], saturation [s], and value [v]. Each
    value is a normalized float [0..1] *)
type hsv = float * float * float

(** [hsl_string_of_hsv hsv] creates a hue-saturation-lightness string
    (according to the CSS3 specification) corresponding to the HSV color
    [hsv]*)
let hsl_string_of_hsv (h, sv, v) =
  let l = v *. (1. -. (sv /. 2.)) in
  let sl =
    if l = 0. || l = 1. then 0. else (v -. l) /. Float.min l (1. -. l)
  in
  Printf.sprintf "hsl(%f, %f%%, %f%%)" (h *. 360.) (sl *. 100.)
    (l *. 100.)

(** [hsv_step hsv] is a state machine which takes [hsv] and outputs a
    new color. Any color outputted will never match an earlier color in
    the output sequence, to the extent permitted by floating-point
    precision loss.*)
let hsv_step (h, s, v) =
  (* PATTERN: hue changes first, then saturation, and finally value *)
  let new_h = h +. 0.30 in
  if new_h > 1. then
    let new_s = s -. 0.30 in
    if new_s < 0. then (new_h -. 1., 1., v /. 2.)
    else (new_h -. 1., new_s, v)
  else (new_h, s, v)

type plot = {
  label : string;
  points : (float * float) list;
  color : hsv;
}

type t = {
  plots : plot list;
  domain : float * float;
  range : float * float;
}

let create domain range = { plots = []; domain; range }

let add_plot label points g =
  let color =
    match g.plots with
    | [] -> (0., 1., 1.) (* initial color - bright red *)
    | h :: _ -> hsv_step h.color
  in
  { g with plots = { label; points; color } :: g.plots }

let span (min, max) = max -. min

let text_of ?fill:(f = "black") c x y txt =
  Container
    ( "text",
      [ ("fill", f); ("class", c); ("x", x); ("y", y) ],
      [ Text txt ] )

let line_of ?fill:(f = "black") c x1 x2 y1 y2 =
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

let circle_of ?fill:(f = "black") c x y r =
  Item
    ( "circle",
      [ ("fill", f); ("class", c); ("cx", x); ("cy", y); ("r", r) ] )

let eqs_label num eq =
  let col = hsl_string_of_hsv eq.color in
  let h = (num * 30) + 90 in
  Container
    ( "g",
      [],
      [
        circle_of "relation_view_disc" ~fill:col "40" (string_of_int h)
          "10";
        text_of "relation_view_text" ~fill:col "60px"
          (string_of_int (h + 5))
          eq.label;
      ] )

let eqs_view g =
  let labels = List.mapi eqs_label (List.rev g.plots) in
  let background =
    Item ("rect", [ ("class", "relation_view_background") ])
  in
  let border = line_of "relation_view_line" "100%" "100%" "0" "100%" in
  let header =
    text_of "relation_view_header" "15px" "40px" "Relations"
  in
  let divider =
    line_of "relation_view_line" "0%" "100%" "60px" "60px"
  in
  let max_label_characters =
    List.fold_left
      (fun maxlen eq -> max maxlen (String.length eq.label))
      0 g.plots
  in
  let width = 60 + max 160 (12 * max_label_characters) in
  let height = 90 + max 160 (30 * List.length g.plots) in
  Container
    ( "svg",
      [ ("viewBox", Printf.sprintf "0 0 %i %i" width height) ],
      background :: border :: header :: divider :: labels )

let plot_view g =
  Container ("svg", [ ("viewBox", "300 0 1000 1000") ], [])

let to_svg filename g =
  let styles = open_in "graph_styles.css" in
  let style_obj =
    Container
      ( "style",
        [],
        [
          Text
            ( read_lines styles |> List.rev
            |> List.fold_left (concat_with "\n") "" );
        ] )
  in
  close_in styles;
  let f = open_out filename in
  let dom =
    Container
      ( "svg",
        [
          ("xmlns", "http://www.w3.org/2000/svg"); ("height", "1000px");
        ],
        [ style_obj; eqs_view g ] )
  in
  output_xml f dom;
  close_out f
