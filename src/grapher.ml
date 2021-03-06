(** Implementation of module [Grapher].*)

open Xmldom
open Svghelpers
open Common
open Graphstyles

(** [hsv (h, s, v)] has hue [h], saturation [s], and value [v]. Each
    value is a normalized float [0..1] *)
type hsv = float * float * float

(** [hsl_string_of_hsv hsv] creates a hue-saturation-lightness string
    (according to the CSS3 specification) corresponding to the HSV color
    [hsv]*)
let hsl_string_of_hsv (h, s, v) =
  (* This formula was taken from Wikipedia and translated into OCaml *)
  (* l = lightness *)
  let l = v *. (1. -. (s /. 2.)) in
  (* s' = saturation for the HSL color *)
  let s' =
    if l = 0. || l = 1. then 0. else (v -. l) /. Float.min l (1. -. l)
  in
  Printf.sprintf "hsl(%f, %f%%, %f%%)" (h *. 360.) (s' *. 100.)
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
    let h_rotated = h' -. 1. in
    let s' = s -. step in
    if s' < 0. then (h_rotated, 1., v /. 2.) else (h_rotated, s', v)
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
    [ratio] is the aspect ratio of a single square unit on the graph.
    RI: [x2 >= x1], [y2 >= y1], [ratio > 0], [ratio] is finite.*)
type t = {
  plots : plot list;
  x_bounds : bounds;
  y_bounds : bounds;
  ratio : float;
}

let create x_bounds y_bounds ratio =
  assert (valid_bounds x_bounds && valid_bounds y_bounds);
  { plots = []; x_bounds; y_bounds; ratio }

let add_plot label segments g =
  let color =
    match g.plots with
    | [] -> (0., 1., 1.) (* initial color - bright red *)
    | h :: _ -> hsv_step h.color
  in
  { g with plots = { label; segments; color } :: g.plots }

(** [svg_height styles plots] calculates the optimal height for the SVG
    as a whole, given that the SVG uses stylesheet [styles] and will
    visualize only the plots in [plots].*)
let svg_height styles plots =
  let body_computed_height =
    (List.length plots + 1) * label_vertical_spacing styles
  in
  header_height styles
  + max (body_min_height styles) body_computed_height

(** [plot_info_width styles plots] calculates the optimal width for the
    plot info box, given that the SVG uses stylesheet [styles] and will
    visualize only the plots in [plots].*)
let plot_info_width s plots =
  let font_size = float_of_int (label_font_size s) in
  (* rough approximation *)
  let char_width = int_of_float (font_size *. 0.6) in
  let max_label_chars =
    List.fold_left
      (fun max_len { label } -> max max_len (String.length label))
      0 plots
  in
  max (body_min_width s)
    (label_indent s + (char_width * max_label_chars))

(** [make_plot_label styles index eq] creates an SVG element serving as
    a label for the equation [eq], given that [eq] was the [index]-th
    equation specified by the user (and thus at the [index]-th position
    in the equation list). The label is positioned and formatted
    according to [styles]. *)
let make_plot_label styles index eq =
  let col = hsl_string_of_hsv eq.color in
  let x = string_of_int (label_indent styles) in
  (* vertical offset from header divider *)
  let header_offset = (index + 1) * label_vertical_spacing styles in
  let y = string_of_int (header_offset + header_height styles) in
  make_group []
    [
      make_circle "plot_info_disc" [ ("fill", col) ] x y "10";
      make_text "plot_info_label" [ ("fill", col) ] x y eq.label;
    ]

(** [make_plot_info styles plots width height] creates an info box
    containing a list of all the graphed relations in [plots] and their
    corresponding colors. It may also contain other information. It has
    a width of [width] pixels and a height of [height] pixels. It is
    formatted according to [styles]. Requires: the values for [width]
    and [height] were derived from [styles] using [plot_info_width] and
    [svg_height].*)
let make_plot_info styles plots width height =
  (* width & height as float *)
  let w_flt, h_flt = (float_of_int width, float_of_int height) in
  (* header height as float *)
  let hh_flt = float_of_int (Graphstyles.header_height styles) in
  make_svg
    [ ("width", string_of_int width); ("height", string_of_int height) ]
    [
      (* background *)
      Item ("rect", [ ("class", "plot_info_background") ]);
      (* header text *)
      make_text "plot_info_header" [] "0" "0" "Relations";
      (* header divider *)
      make_polyline "plot_info_border" []
        [ (0., hh_flt); (w_flt, hh_flt) ];
      (* border for the equation box *)
      make_region_border "plot_info_border" [] (0., 0.) (w_flt, h_flt);
      (* all the actual labels *)
      make_group []
        (List.mapi (make_plot_label styles) (List.rev plots));
    ]

(** [graph_viewbox g] creates a coordinate system for an SVG element
    such that the SVG element's top-left coordinate is the top-left
    point within the bounds of [g] and its bottom-right coordinate is
    the bottom-right point within the bounds of [g], etc.*)
let graph_viewbox g =
  Printf.sprintf "%f %f %f %f" (fst g.x_bounds) (fst g.y_bounds)
    (span g.x_bounds) (span g.y_bounds)

(** [make_segment color segment] draws the list of points [segment] so
    that each point in the list is connected to the next point with a
    straight line. The drawn line has color [color] and has class
    ["graph_path"].*)
let make_segment color segment =
  make_polyline "graph_path"
    [ ("stroke", hsl_string_of_hsv color) ]
    segment

(** [make_plot p] creates an element representing the graph of plot [p].*)
let make_plot p =
  List.map (make_segment p.color) p.segments |> make_group []

let increment_to_coords pos_bound neg_bound increment =
  let rec convert_half rel acc bound increment_c =
    match acc with
    | h :: t ->
        if rel (h +. increment_c) bound then h :: t
        else
          convert_half rel
            ((h +. increment_c) :: h :: t)
            bound increment_c
    | _ -> failwith "Invalid list supplied"
  in
  convert_half ( < ) [ 0. ] neg_bound (0. -. increment)
  @ (convert_half ( > ) [ 0. ] pos_bound increment |> List.rev)
  |> List.sort_uniq Stdlib.compare

let compute_error (increment : float) : float =
  abs_float (Float.round increment -. increment)

let rec generate_increments range acc line_count_c =
  let increment_base_1 = log10 (range /. float_of_int line_count_c) in
  let increment_base_2 = increment_base_1 -. log10 2. in
  let increment_base_5 = increment_base_1 -. log10 5. in
  let error_base_1 = compute_error increment_base_1 in
  let error_base_2 = compute_error increment_base_2 in
  let error_base_5 = compute_error increment_base_5 in
  let selected_increment =
    if error_base_1 < error_base_2 && error_base_1 < error_base_5 then
      (10. ** Float.round increment_base_1, error_base_1)
    else if error_base_2 < error_base_1 && error_base_2 < error_base_5
    then (2. *. (10. ** Float.round increment_base_2), error_base_2)
    else (5. *. (10. ** Float.round increment_base_5), error_base_5)
  in
  match selected_increment with
  | increment, error ->
      filter_increments line_count_c acc increment error range

and filter_increments line_count_c acc increment error range =
  if line_count_c <= 2 then
    if List.length acc = 0 then [ increment ] else acc
  else if increment < range && increment <> 0. then
    if error <> 0. then
      generate_increments range (increment :: acc) (line_count_c - 1)
    else [ increment ]
  else generate_increments range acc (line_count_c - 1)

let compute_increment range line_count =
  List.hd (List.rev (generate_increments range [] line_count))

(** [get_grid_pos (x_min, x_max) (y_min, y_max)] returns a tuple of
    float lists [(horiz,vert)], where [horiz] is a list of Y coordinates
    to draw each horizontal gridline at and [vert] is a list of X
    coordinates to draw each vertical gridline at. Requires:
    [x_min < x_max], [y_min < y_max], all inputs are finite. *)
let get_grid_pos
    ((x_min, x_max) : float * float)
    ((y_min, y_max) : float * float) : float list * float list =
  let y_max_line_count = 20 in
  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in
  ( increment_to_coords y_max y_min
      (compute_increment y_range y_max_line_count),
    increment_to_coords x_max x_min
      (compute_increment x_range
         (int_of_float
            (Float.round
               (x_range /. y_range *. float_of_int y_max_line_count))))
  )

(** [ff_float_str fl] converts [fl] to a string. If the string
    representation ends with a ['.'], it is discarded. This is because
    Firefox does not know how to interpret values of the form [2.].*)
let ff_float_str fl =
  let s = string_of_float fl in
  match String.index_opt s '.' with
  | Some i when i = String.length s - 1 -> String.sub s 0 i
  | _ -> s

let make_gridline_step atts spacing f ((lines, labels), count) v =
  let x, y, text, coords = f v in
  let new_labels, style =
    if count mod spacing = 0 && v <> 0. then
      ( make_text "graph_gridline_label" atts x y text :: labels,
        "graph_gridline_bold" )
    else (labels, "graph_gridline")
  in
  ((make_polyline style [] coords :: lines, new_labels), count + 1)

let make_gridlines font_size spacing ratio f lst =
  let atts =
    [
      ("font-size", ff_float_str font_size);
      (* stroke specified as Safari bugfix *)
      ("stroke-width", ff_float_str (font_size *. 0.05));
      (* we need to invert the Y scale, and adjust for aspect ratio. 0*)
      ("transform", Printf.sprintf "scale(%f, -1)" (1. /. ratio));
    ]
  in
  List.fold_left (make_gridline_step atts spacing f) (([], []), 0) lst
  |> fst

let horiz_gridline_info (x1, x2) font_size v =
  let s = ff_float_str v in
  (* the math here offsets the x coordinate to the LEFT of the Y axis *)
  let x = float_of_int (String.length s + 1) *. font_size *. -0.5 in
  (* the math here lowers the y position by an eyeballed amount so that
     the text is roughly centered on the line *)
  let y = ~-.(v -. (font_size *. 0.35)) in
  (ff_float_str x, ff_float_str y, s, [ (x1, v); (x2, v) ])

let vert_gridline_info (y1, y2) font_size ratio v =
  let s = ff_float_str v in
  (* the math here roughly centers the X position on the gridline *)
  let x =
    (v *. ratio) -. (float_of_int (String.length s) *. font_size *. 0.25)
  in
  (* y offset of font_size places text under the X axis *)
  let y = font_size in
  (ff_float_str x, ff_float_str y, s, [ (v, y1); (v, y2) ])

(** [make_gridlines (x1, x2) (y1, y2) ratio] returns [(ll, la)] where
    [ll] is an element representing the gridlines and [la] is an element
    representing the gridline labels of a graph with X bounds (x1, x2)
    and Y bounds (y1, y2) and aspect ratio [ratio]. Requires: [x2 > x1],
    [y2 > y1], [+inf > aspect > 0] and all inputs are finite.*)
let make_gridlines x_b y_b r =
  let horiz_pre, vert_pre = get_grid_pos x_b y_b in
  let font_size, spacing = (0.03 *. span y_b, 2) in
  let f_h = horiz_gridline_info x_b font_size in
  let f_v = vert_gridline_info y_b font_size r in
  let hlines, hlabels =
    make_gridlines font_size spacing r f_h horiz_pre
  in
  let vlines, vlabels =
    make_gridlines font_size spacing r f_v vert_pre
  in
  ( make_group [] (List.rev_append hlines vlines),
    make_group [] (List.rev_append hlabels vlabels) )

let make_graph_components graph =
  let (x1, x2), (y1, y2) = (graph.x_bounds, graph.y_bounds) in
  let gridlines, gridlabels =
    make_gridlines graph.x_bounds graph.y_bounds graph.ratio
  in
  [
    (* graph BG (not really necessary, but why not have one)*)
    Item ("rect", [ ("class", "graph_background") ]);
    gridlines;
    (*axes*)
    make_polyline "graph_axis" [] [ (x1, 0.); (x2, 0.) ];
    make_polyline "graph_axis" [] [ (0., y1); (0., y2) ];
    (* the actual plots *)
    make_group [] (List.map make_plot graph.plots);
    gridlabels;
    (* border for the graph (draw on top of everything) *)
    make_region_border "graph_border" [] (x1, y1) (x2, y2);
  ]

(** [make_graph g x_offset width height] creates an SVG element
    representing a visual graph of [g]. The element is offset from the
    left on the X axis by [x_offset] pixels, and has a width of [width]
    pixels and height of [height] pixels.*)
let make_graph g x w h =
  make_group
    (* Here, the SVG wrapped is wrapped in transformed <g> because
       Chromium is bugged and doesn't support transform on SVG. See:
       https://www.w3.org/TR/SVG2/struct.html#SVGElement *)
    [ ("transform", "matrix(1 0 0 -1 0 " ^ string_of_int h ^ ")") ]
    [
      Comment
        "svg element should support transform, but this is broken in \
         Chromium; we wrap the graph view as a workaround";
      make_svg
        [
          ("x", string_of_int x);
          ("width", string_of_int w);
          ("height", string_of_int h);
          ("viewBox", graph_viewbox g);
          ("preserveAspectRatio", "none");
        ]
        (make_graph_components g);
    ]

(** [safe_load_styles filename] attempts to load the stylesheet in
    [filename]. On success, returns the appropriate [Graphstyles.t]; on
    failure, prints an error message to stderr and terminates execution
    with error code 1.*)
let safe_load_styles filename =
  match load filename with
  | Ok s -> s
  | Error e ->
      Io.print_error (e ^ "\n");
      exit 1

let to_svg filename g =
  let styles = safe_load_styles "graph_styles.css" in
  (* total height of the resulting SVG *)
  let svg_height = svg_height styles g.plots in
  (* width of the plot info box *)
  let plot_info_width = plot_info_width styles g.plots in
  let default_ratio = span g.x_bounds /. span g.y_bounds in
  (* width of the graph part (not the whole image )*)
  let graph_width =
    int_of_float (float_of_int svg_height *. default_ratio *. g.ratio)
  in
  (* begin export *)
  let f = open_out filename in
  make_svg
    [
      ("xmlns", "http://www.w3.org/2000/svg");
      ("width", string_of_int (plot_info_width + graph_width));
      ("height", string_of_int svg_height);
    ]
    [
      Container ("style", [], [ Text (raw_stylesheet styles) ]);
      make_plot_info styles g.plots plot_info_width svg_height;
      make_graph g plot_info_width graph_width svg_height;
    ]
  |> output_xml f;
  close_out f
