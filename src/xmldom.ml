type tag = string

type attribute = string * string

type element =
  | Text of string
  | Comment of string
  | Item of tag * attribute list
  | Container of tag * attribute list * element list

let string_of_attributes =
  List.fold_left
    (fun acc (k, v) -> Printf.sprintf "%s %s=\"%s\"" acc k v)
    ""

let rec output_xml f ?tab_level:(tl = 0) =
  let tabs = String.make tl '\t' in
  function
  | Text s -> Printf.fprintf f "%s" s
  | Comment s -> Printf.fprintf f "<!--%s-->" s
  | Item (e, o) ->
      Printf.fprintf f "%s<%s %s/>\n" tabs e (string_of_attributes o)
  | Container (e, o, c) ->
      Printf.fprintf f "%s<%s %s>\n" tabs e (string_of_attributes o);
      List.iter (output_xml ~tab_level:(tl + 1) f) c;
      Printf.fprintf f "%s</%s>\n" tabs e
