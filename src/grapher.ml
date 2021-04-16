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

let to_svg filename g = failwith "Unimplemented"
