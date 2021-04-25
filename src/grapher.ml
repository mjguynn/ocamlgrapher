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
  let step = 0.3 in
  (* PATTERN: hue changes first, then saturation, and finally value *)
  let h' = h +. step in
  if h' > 1. then
    let h'' = h' -. 1. in
    let s' = s -. step in
    if s' < 0. then (h'', 1., v /. 2.) else (h'', s', v)
  else (h', s, v)

(** A [plot] contains information about a plot of one or more continous
    line segments. [label] is a human-readable identifier for the plot,
    such as its formula. [segments] is a list of plot segments for the
    plot. [color] is the HSV color assigned to this plot on the graph. *)
type plot = {
  label : string;
  segments : points list;
  color : hsv;
}

(** AF: Let [(x1, x2) = x_bounds] and [(y1, y2) = y_bounds]. Then an
    instance of type [t] represents a graph of the plots [plots] on a
    window spanning [x1..x2] on the X-axis and [y1..y2] on the Y-axis.

    RI: [x2 >= x1] and [y2 >= y1]. *)
type t = {
  plots : plot list;
  x_bounds : float * float;
  y_bounds : float * float;
}

let create x_bounds y_bounds =
  assert (valid_bounds x_bounds && valid_bounds y_bounds);
  { plots = []; x_bounds; y_bounds }

let add_plot label segments g =
  let color =
    match g.plots with
    | [] -> (0., 1., 1.) (* initial color - bright red *)
    | h :: _ -> hsv_step h.color
  in
  { g with plots = { label; segments; color } :: g.plots }

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

type config = {
  header_height : int;
  body_min_width : int;
  body_min_height : int;
  label_vertical_spacing : int;
  label_indent : int;
  label_font_size : int;
}

let styles_from_stylesheet i =
  let styles =
    read_lines i |> List.fold_left (fun acc s -> s ^ "\n" ^ acc) ""
  in
  seek_in i 0;
  styles

let plot_info_height c g =
  c.header_height
  + max c.body_min_height
      ((List.length g.plots + 1) * c.label_vertical_spacing)

let plot_info_width c g =
  (* rough approximation *)
  let char_width =
    int_of_float (float_of_int c.label_font_size *. 0.6)
  in
  let max_label_chars =
    List.fold_left
      (fun maxlen eq -> max maxlen (String.length eq.label))
      0 g.plots
  in
  max c.body_min_width (c.label_indent + (char_width * max_label_chars))

let make_plot_label c i eq =
  let col = hsl_string_of_hsv eq.color in
  let x = string_of_int c.label_indent in
  let y =
    ((i + 1) * c.label_vertical_spacing) + c.header_height
    |> string_of_int
  in
  Container
    ( "g",
      [],
      [
        create_circle "plot_info_disc" ~fill:col x y "10";
        create_text "plot_info_label" ~fill:col x y eq.label;
      ] )

let make_plot_info c g =
  let labels = List.mapi (make_plot_label c) (List.rev g.plots) in
  let background =
    Item ("rect", [ ("class", "plot_info_background") ])
  in
  let border = create_line "plot_info_line" "100%" "100%" "0" "100%" in
  let header = create_text "plot_info_header" "0" "0" "Relations" in
  let divider =
    let h = string_of_int c.header_height in
    create_line "plot_info_line" "0" "100%" h h
  in
  background :: border :: header :: divider :: labels

let config_from_stylesheet i =
  let base_cfg =
    {
      header_height = 0;
      body_min_width = 0;
      body_min_height = 0;
      label_vertical_spacing = 0;
      label_indent = 0;
      label_font_size = 0;
    }
  in
  let rec parse_line acc =
    try
      match
        Scanf.sscanf (input_line i) " --%s@: %upx" (fun v n -> (v, n))
      with
      | "header-height", header_height ->
          parse_line { acc with header_height }
      | "body-min-width", body_min_width ->
          parse_line { acc with body_min_width }
      | "body-min-height", body_min_height ->
          parse_line { acc with body_min_height }
      | "label-vertical-spacing", label_vertical_spacing ->
          parse_line { acc with label_vertical_spacing }
      | "label-indent", label_indent ->
          parse_line { acc with label_indent }
      | "label-font-size", label_font_size ->
          parse_line { acc with label_font_size }
      | s, _ -> parse_line acc
      | exception Scanf.Scan_failure _ -> parse_line acc
    with End_of_file -> acc
  in
  let parsed_cfg = parse_line base_cfg in
  seek_in i 0;
  parsed_cfg

let graph_viewbox g =
  Printf.sprintf "%f %f %f %f" (fst g.x_bounds) (fst g.y_bounds)
    (span g.x_bounds) (span g.y_bounds)

let create_polyline stroke c points =
  let coords =
    List.fold_left
      (fun acc (x, y) ->
        Printf.sprintf "%s %f,%f" acc x (Common.flip y))
      "" points
  in
  Item
    ( "polyline",
      [ ("stroke", stroke); ("class", c); ("points", coords) ] )

let make_plot p =
  Container
    ( "g",
      [],
      List.map
        (create_polyline (hsl_string_of_hsv p.color) "graph_path")
        p.segments )

let make_graph g = List.map make_plot g.plots

let to_svg filename g =
  (* load stylesheet, create DOM element *)
  let stylesheet = open_in "graph_styles.css" in
  let config = config_from_stylesheet stylesheet in
  let styles =
    Container ("style", [], [ Text (styles_from_stylesheet stylesheet) ])
  in
  close_in stylesheet;
  let height = plot_info_height config g in
  let plot_info_width = plot_info_width config g in
  let plot_info =
    Container
      ( "svg",
        [
          ("width", string_of_int plot_info_width);
          ("height", string_of_int height);
        ],
        make_plot_info config g )
  in
  let graph_ratio = span g.x_bounds /. span g.y_bounds in
  let graph_width = int_of_float (float_of_int height *. graph_ratio) in
  let graph =
    Container
      ( "svg",
        [
          ("x", string_of_int plot_info_width);
          ("width", string_of_int graph_width);
          ("height", string_of_int height);
          ("viewBox", graph_viewbox g);
        ],
        make_graph g )
  in
  let dom =
    Container
      ( "svg",
        [ ("xmlns", "http://www.w3.org/2000/svg") ],
        [ styles; plot_info; graph ] )
  in
  (* begin export *)
  let f = open_out filename in
  output_xml f dom;
  close_out f
