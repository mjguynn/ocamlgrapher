(** Implementation of module [Graphstyles]. *)

(** AF: Represents the parsed information from a stylesheet, where
    [stylesheet] is the unaltered text of that stylesheet and each entry
    [(s,i)] in [variables] is a CSS variable titled [s] which is bound
    to a pixel value of [i]. [variables] is an association list, so if
    multiple entries have the same [s], the first one in the list takes
    priority.

    RI: Must have entries [(s,i)] in [variables] for AT LEAST these [s]:
    ["--header-height"], ["--body-min-width"], ["--body-min-height"],
    ["--label-vertical-spacing"], ["--label-indent"],
    ["--label-font-size"]*)
type t = {
  stylesheet : string;
  variables : (string * int) list;
}

(** [rep_ok s] takes [s] as input. If [s]'s representation invariant is
    OK, then it returns [Ok s]; otherwise, it returns an [Error e] where
    [e] is an error message about what part of the RI was violated.*)
let rep_ok s =
  let check_var str = function
    | Error e -> Error e
    | Ok s ->
        if List.mem_assoc str s.variables then Ok s
        else Error ("Stylesheet missing required variable " ^ str)
  in
  Ok s
  |> check_var "header-height"
  |> check_var "body-min-width"
  |> check_var "body-min-height"
  |> check_var "label-vertical-spacing"
  |> check_var "label-indent"
  |> check_var "label-font-size"

(** [load_internal ic] functions the same as [load], except uses an
    already-open [in_channel] instead of opening one from a filename. *)
let load_internal ic =
  let empty_cfg = { stylesheet = ""; variables = [] } in
  let rec parse_line acc =
    match
      Scanf.sscanf (input_line ic) " --%s@: %upx" (fun v n -> (v, n))
    with
    | s, i ->
        parse_line { acc with variables = (s, i) :: acc.variables }
    | exception Scanf.Scan_failure _ -> parse_line acc
    | exception End_of_file -> acc
  in
  let parsed_cfg = parse_line empty_cfg in
  seek_in ic 0;
  let stylesheet =
    Io.read_lines ic |> List.fold_left (fun acc s -> s ^ "\n" ^ acc) ""
  in
  rep_ok { parsed_cfg with stylesheet }

let load filename =
  try
    let ic = open_in filename in
    let res = load_internal ic in
    close_in ic;
    res
  with Sys_error e -> Error ("Error opening stylesheet: " ^ e)

let raw_stylesheet t = t.stylesheet

let header_height t = List.assoc "header-height" t.variables

let body_min_width t = List.assoc "body-min-width" t.variables

let body_min_height t = List.assoc "body-min-height" t.variables

let label_vertical_spacing t =
  List.assoc "label-vertical-spacing" t.variables

let label_indent t = List.assoc "label-indent" t.variables

let label_font_size t = List.assoc "label-font-size" t.variables
