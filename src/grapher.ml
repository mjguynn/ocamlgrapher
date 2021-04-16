type plot = {
  label : string;
  points : (float * float) list;
}

type t = {
  plots : plot list;
  domain : float * float;
  range : float * float;
}

let create domain range = { plots = []; domain; range }

let add_plot label points g =
  { g with plots = { label; points } :: g.plots }

type domopts = (string * string) list

type dom =
  | DOMSingle of string * domopts
  | DOMContainer of string * domopts * dom list

let domopts_to_string =
  List.fold_left
    (fun acc (k, v) -> Printf.sprintf "%s %s=\"%s\"" acc k v)
    ""

let rec write_dom f ?tab_level:(tl = 0) =
  let tabs = String.make tl '\t' in
  function
  | DOMSingle (e, o) ->
      Printf.fprintf f "%s<%s %s/>\n" tabs e (domopts_to_string o)
  | DOMContainer (e, o, c) ->
      Printf.fprintf f "%s<%s %s>\n" tabs e (domopts_to_string o);
      List.iter (write_dom ~tab_level:(tl + 1) f) c;
      Printf.fprintf f "%s</%s>\n" tabs e

let to_svg filename g =
  let f = open_out filename in
  let dom =
    DOMContainer
      ( "svg",
        [
          ("version", "1.1");
          ("baseProfile", "full");
          ("width", "1000");
          ("height", "1000");
          ("xmlns", "http://www.w3.org/2000/svg");
        ],
        [
          DOMSingle
            ( "rect",
              [ ("width", "100%"); ("height", "100%"); ("fill", "red") ]
            );
        ] )
  in
  write_dom f dom;
  close_out f
