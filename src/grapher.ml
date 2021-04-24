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
  Printf.sprintf "hsl(%f, %f%%, %f%%)" (h *. 360.) sl l

(** [hsv_step hsv] is a state machine which takes [hsv] and outputs a
    new color. Any color outputted will never match an earlier color in
    the output sequence, to the extent permitted by floating-point
    precision loss.*)
let hsv_step (h, s, v) =
  (* PATTERN: hue changes first, then saturation, and finally value *)
  let new_h = h +. 0.30 in
  if new_h > 1. then
    let new_s = s -. 0.5 in
    if new_s < 0. then (new_h -. 1., new_s +. 1., v /. 2.)
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

let to_svg filename g =
  let f = open_out filename in
  let dom =
    Container
      ( "svg",
        [ ("xmlns", "http://www.w3.org/2000/svg") ],
        [
          Comment "what is updog";
          Item
            ( "rect",
              [ ("width", "100%"); ("height", "100%"); ("fill", "red") ]
            );
          Container
            ( "text",
              [ ("x", "20"); ("y", "20") ],
              [ Text "testing testing testing" ] );
        ] )
  in
  output_xml f dom;
  close_out f
