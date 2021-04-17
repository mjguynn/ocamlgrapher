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

type dom_tag = string

type dom_attribute = string * string

type dom_element =
  | DOMText of string
  | DOMSingle of dom_tag * dom_attribute list
  | DOMContainer of dom_tag * dom_attribute list * dom_element list

let dom_string_of_attributes =
  List.fold_left
    (fun acc (k, v) -> Printf.sprintf "%s %s=\"%s\"" acc k v)
    ""

let rec dom_output f ?tab_level:(tl = 0) =
  let tabs = String.make tl '\t' in
  function
  | DOMText s -> Printf.fprintf f "%s" s
  | DOMSingle (e, o) ->
      Printf.fprintf f "%s<%s %s/>\n" tabs e
        (dom_string_of_attributes o)
  | DOMContainer (e, o, c) ->
      Printf.fprintf f "%s<%s %s>\n" tabs e (dom_string_of_attributes o);
      List.iter (dom_output ~tab_level:(tl + 1) f) c;
      Printf.fprintf f "%s</%s>\n" tabs e

let span (min, max) = max -. min

let to_svg filename g =
  let f = open_out filename in
  let dom =
    DOMContainer
      ( "svg",
        [ ("xmlns", "http://www.w3.org/2000/svg") ],
        [
          DOMSingle
            ( "rect",
              [ ("width", "100%"); ("height", "100%"); ("fill", "red") ]
            );
          DOMContainer
            ( "text",
              [ ("x", "20"); ("y", "20") ],
              [ DOMText "testing testing testing" ] );
        ] )
  in
  dom_output f dom;
  close_out f
