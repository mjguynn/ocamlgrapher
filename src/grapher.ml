open Xmldom

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

let text_of txt styles = Container ("text", styles, [ Text txt ])

let line_of s x1 x2 y1 y2 =
  Item
    ( "line",
      [ ("class", s); ("x1", x1); ("x2", x2); ("y1", y1); ("y2", y2) ]
    )

let equation_view_stylesheet =
  {|
    .equation_view_background {
      fill: #EFEFF4;
      width: 100%;
      height: 100%;
    }
    .equation_view_line {
      stroke: #999;
      stoke-width: 4px;
    }
    .equation_view_text {
      font-family: verdana;
      font-size: 30px;
      fill: #444;
    }
    .equation_view_equation {
      font-family: consolas;
      font-size: 20px;
      stroke: #444;
      stroke-width: 1px;
    }
    .equation_view_disc {
      stroke: #444;
      stroke-width: 1px;
    }
  |}

let eqs_label num eq =
  let cstr = hsl_string_of_hsv eq.color in
  let h = (num * 30) + 90 in
  Container
    ( "g",
      [],
      [
        Item
          ( "circle",
            [
              ("fill", cstr);
              ("cx", "40px");
              ("cy", string_of_int h);
              ("r", "10px");
              ("class", "equation_view_disc");
            ] );
        text_of eq.label
          [
            ("x", "60px");
            ("y", string_of_int (h + 5));
            ("fill", cstr);
            ("class", "equation_view_equation");
          ];
      ] )

let eqs_view g =
  let style =
    Container ("style", [], [ Text equation_view_stylesheet ])
  in
  let labels = List.mapi eqs_label (List.rev g.plots) in
  let background =
    Item ("rect", [ ("class", "equation_view_background") ])
  in
  let border = line_of "equation_view_line" "100%" "100%" "0" "100%" in
  let header =
    text_of "Equations"
      [ ("class", "equation_view_text"); ("x", "15px"); ("y", "40px") ]
  in
  let divider =
    line_of "equation_view_line" "0%" "100%" "60px" "60px"
  in
  Container
    ( "svg",
      [ ("viewBox", "0 0 300 1000") ],
      style :: background :: border :: header :: divider :: labels )

let to_svg filename g =
  let f = open_out filename in
  let dom =
    Container
      ( "svg",
        [
          ("xmlns", "http://www.w3.org/2000/svg"); ("height", "1000px");
        ],
        [ eqs_view g ] )
  in
  output_xml f dom;
  close_out f
