(** Implementation of module [Grapher].*)

open Xmldom
open Svghelpers
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

type config = {
  header_height : int;
  body_min_width : int;
  body_min_height : int;
  label_vertical_spacing : int;
  label_indent : int;
  label_font_size : int;
}

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

let styles_from_stylesheet i =
  let styles =
    Io.read_lines i |> List.fold_left (fun acc s -> s ^ "\n" ^ acc) ""
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
        make_circle "plot_info_disc" [ ("fill", col) ] x y "10";
        make_text "plot_info_label" [ ("fill", col) ] x y eq.label;
      ] )

let make_plot_info c g w h =
  let labels = List.mapi (make_plot_label c) (List.rev g.plots) in
  let background =
    Item ("rect", [ ("class", "plot_info_background") ])
  in
  let header = make_text "plot_info_header" [] "0" "0" "Relations" in
  let hh = float_of_int c.header_height in
  let divider =
    make_polyline "plot_info_border" []
      [ (0., hh); (float_of_int w, hh) ]
  in
  Container
    ( "svg",
      [ ("width", string_of_int w); ("height", string_of_int h) ],
      [
        background;
        header;
        divider;
        make_region_border "plot_info_border"
          [ ("id", "plot-info-border") ]
          (0., 0.)
          (float_of_int w, float_of_int h);
      ]
      @ labels )

let graph_viewbox g =
  Printf.sprintf "%f %f %f %f" (fst g.x_bounds)
    (flip (snd g.y_bounds))
    (span g.x_bounds) (span g.y_bounds)

let invert_y = List.map (fun (x, y) -> (x, flip y))

let make_plot p =
  Container
    ( "g",
      [],
      List.map
        (fun l ->
          make_polyline "graph_path"
            [ ("stroke", hsl_string_of_hsv p.color) ]
            (invert_y l))
        p.segments )

(** [get_grid_pos x_min x_max y_min y_max] returns a tuple of float
    lists, with the first float list being the positions to draw the
    vertical lines and the second float list being the positions to draw
    the horizontal lines *)
let get_grid_pos
    (x_min : float)
    (x_max : float)
    (y_min : float)
    (y_max : float) : float list * float list =
  let y_max_line_count = 10 in

  let y_axis_endpoint = 10. in

  let abs_floor (num : float) : float =
    if num > 0. then floor num else ceil num
  in

  let compute_error (increment : float) : float =
    abs_float (Float.round increment -. increment)
  in

  (* Discard if increment 0 or greater than range *)
  let rec compute_increment range line_count =
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
    let selected_increment =
      if error_1 < error_2 && error_1 < error_5 then
        10. ** Float.round increment_1
      else if error_2 < error_1 && error_2 < error_5 then
        2. *. (10. ** Float.round increment_2)
      else 5. *. (10. ** Float.round increment_5)
    in
    match line_count with
    | 2 -> (2, selected_increment)
    | n ->
        if selected_increment < range then (n, selected_increment)
        else compute_increment range (n - 1)
  in

  let rec increment_to_coords acc increment =
    match acc with
    | h :: t ->
        if h -. increment <= 0. then h :: t
        else increment_to_coords ((h -. increment) :: h :: t) increment
    | _ -> failwith "Invalid list supplied"
  in

  let x_range = abs_floor x_max -. abs_floor x_min in
  let y_range = abs_floor y_max -. abs_floor y_min in
  let x_range_scale = x_range /. y_range in
  let x_axis_endpoint = y_axis_endpoint *. x_range_scale in
  ( begin
      match
        compute_increment x_range
          (int_of_float
             (Float.round
                (x_range_scale *. float_of_int y_max_line_count)))
      with
      | n, k ->
          increment_to_coords [ x_axis_endpoint ]
            (x_axis_endpoint /. float_of_int n)
    end,
    match compute_increment y_range y_max_line_count with
    | n, h ->
        increment_to_coords [ y_axis_endpoint ]
          (y_axis_endpoint /. float_of_int n) )

(* helper method that returns a make_polyline command. For this, graphs
   the vertical gridlines. *)
let rec vert_grids_draw (x1, x2) (y1, y2) vert_coords acc =
  match vert_coords with
  | [] -> acc
  | h :: tail ->
      vert_grids_draw (x1, x2) (y1, y2) tail
        (make_polyline "graph_gridline" [] [ (h, y1); (h, y2) ] :: acc)

(* helper method that returns a make_polyline command. For this, graphs
   the horizontal gridlines. *)
let rec hor_grids_draw (x1, x2) (y1, y2) hor_coords vert_coords acc =
  match hor_coords with
  | [] -> vert_grids_draw (x1, x2) (y1, y2) vert_coords acc
  | h :: tail ->
      hor_grids_draw (x1, x2) (y1, y2) tail vert_coords
        (make_polyline "graph_gridline" [] [ (x1, h); (x2, h) ] :: acc)

let make_graph g x w h =
  let (x1, x2), (y1, y2) = (g.x_bounds, g.y_bounds) in
  let background = Item ("rect", [ ("class", "graph_background") ]) in
  let axes =
    Container
      ( "g",
        [ ("id", "graph-axes") ],
        [
          make_polyline "graph_axis" [] [ (x1, 0.); (x2, 0.) ];
          make_polyline "graph_axis" [] [ (0., flip y1); (0., flip y2) ];
        ] )
  in
  (* X & Y Axis *)
  Container
    ( "svg",
      [
        ("x", string_of_int x);
        ("width", string_of_int w);
        ("height", string_of_int h);
        ("viewBox", graph_viewbox g);
      ],
      let hor_bars, vert_bars = get_grid_pos x1 x2 y1 y2 in
      background
      :: hor_grids_draw (x1, x2) (y1, y2) hor_bars vert_bars []
      @ (axes :: List.map make_plot g.plots)
      @ [
          make_region_border "graph_border"
            [ ("id", "graph-border") ]
            (x1, flip y1)
            (x2, flip y2);
        ] )

let to_svg filename g =
  (* load stylesheet, create DOM element *)
  let stylesheet = open_in "graph_styles.css" in
  let config = config_from_stylesheet stylesheet in
  let styles =
    Container ("style", [], [ Text (styles_from_stylesheet stylesheet) ])
  in
  close_in stylesheet;
  (* set up graph *)
  let height = plot_info_height config g in
  let plot_info_width = plot_info_width config g in
  let plot_info = make_plot_info config g plot_info_width height in
  let graph_ratio = span g.x_bounds /. span g.y_bounds in
  let graph_width = int_of_float (float_of_int height *. graph_ratio) in
  let graph = make_graph g plot_info_width graph_width height in
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
