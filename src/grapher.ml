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

(** [get_grid_pos x_min x_max y_min y_max] returns a tuple of float
    lists [(horiz,vert)], where [horiz] is a list of Y coordinates to
    draw each horizontal gridline at and [vert] is a list of X
    coordinates to draw each vertical gridline at. Requires:
    [x_min < x_max], [y_min < y_max], all inputs are finite. *)
let get_grid_pos
    (x_min : float)
    (x_max : float)
    (y_min : float)
    (y_max : float) : float list * float list =
  let y_max_line_count = 20 in

  let abs_floor (num : float) : float =
    if num > 0. then floor num else ceil num
  in

  let compute_error (increment : float) : float =
    abs_float (Float.round increment -. increment)
  in

  (* Discard if increment 0 or greater than range *)
  let compute_increment range line_count =
    let rec generate_increments acc line_count_c =
      (* possible increment of multiple of 10^n *)
      let increment_1 = log10 (range /. float_of_int line_count_c) in
      (* possible increment of multiple of 2(10^n) *)
      let increment_2 = increment_1 -. log10 2. in
      (* possible increment of multiple of 5(10^n) *)
      let increment_5 = increment_1 -. log10 5. in
      let error_1 = compute_error increment_1 in
      let error_2 = compute_error increment_2 in
      let error_5 = compute_error increment_5 in
      let selected_increment =
        if error_1 < error_2 && error_1 < error_5 then
          (10. ** Float.round increment_1, error_1)
        else if error_2 < error_1 && error_2 < error_5 then
          (2. *. (10. ** Float.round increment_2), error_2)
        else (5. *. (10. ** Float.round increment_5), error_5)
      in
      match line_count_c with
      | 2 -> acc
      | n -> (
          match selected_increment with
          | increment, error ->
              if increment < range && increment <> 0. then
                if error <> 0. then
                  generate_increments (increment :: acc) (n - 1)
                else [ increment ]
              else generate_increments acc (n - 1) )
    in
    List.hd (List.rev (generate_increments [] line_count))
  in

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
    List.merge
      (fun x y -> 0)
      (convert_half ( > ) [ 0. ] pos_bound increment)
      (convert_half ( < ) [ 0. ] neg_bound (0. -. increment))
  in

  let x_range = abs_floor x_max -. abs_floor x_min in
  let y_range = abs_floor y_max -. abs_floor y_min in
  ( increment_to_coords y_max y_min
      (compute_increment y_range y_max_line_count),
    increment_to_coords x_max x_min
      (compute_increment x_range
         (int_of_float
            (Float.round
               (x_range /. y_range *. float_of_int y_max_line_count))))
  )

(** [make_gridlines (x1, x2) (y1, y2)] creates an element representing
    the gridlines of a graph with X bounds (x1, x2) and Y bounds (y1,
    y2). Requires: [x2 > x1], [y2 > y1], and all inputs are finite.*)
let make_gridlines (x1, x2) (y1, y2) =
  let make_line f =
    List.fold_left
      (fun acc v -> make_polyline "graph_gridline" [] (f v) :: acc)
      []
  in
  let make_horiz_lines = make_line (fun y -> [ (x1, y); (x2, y) ]) in
  let make_vert_lines = make_line (fun x -> [ (x, y1); (x, y2) ]) in
  let hbars, vbars = get_grid_pos x1 x2 y1 y2 in
  make_group []
    (List.flatten [ make_horiz_lines hbars; make_vert_lines vbars ])

(** [make_graph g x_offset width height] creates an SVG element
    representing a visual graph of [g]. The element is offset from the
    left on the X axis by [x_offset] pixels, and has a width of [width]
    pixels and height of [height] pixels.*)
let make_graph g x w h =
  let (x1, x2), (y1, y2) = (g.x_bounds, g.y_bounds) in
  let axes =
    make_group []
      [
        make_polyline "graph_axis" [] [ (x1, 0.); (x2, 0.) ];
        make_polyline "graph_axis" [] [ (0., y1); (0., y2) ];
      ]
  in
  make_svg
    [
      ("x", string_of_int x);
      ("width", string_of_int w);
      ("height", string_of_int h);
      ("viewBox", graph_viewbox g);
      ("preserveAspectRatio", "none");
      ("transform", "matrix(1 0 0 -1 0 " ^ string_of_int h ^ ")");
    ]
    [
      (* graph BG (not really necessary, but why not have one)*)
      Item ("rect", [ ("class", "graph_background") ]);
      (* gridlines *)
      make_gridlines g.x_bounds g.y_bounds;
      (* axes (draw on top of gridlines) *)
      axes;
      (* the actual plots *)
      make_group [] (List.map make_plot g.plots);
      (* border for the graph (draw on top of everything) *)
      make_region_border "graph_border" [] (x1, y1) (x2, y2);
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
    [ ("xmlns", "http://www.w3.org/2000/svg") ]
    [
      Container ("style", [], [ Text (raw_stylesheet styles) ]);
      make_plot_info styles g.plots plot_info_width svg_height;
      make_graph g plot_info_width graph_width svg_height;
    ]
  |> output_xml f;
  close_out f
