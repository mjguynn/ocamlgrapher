(** Provides functionality to safely create and manipulate XML DOM
    trees. *)

(** A [tag] is the "type" of a DOM element. For example, the tag of
    <code><p></code> is ["p"] and the tag of <code><img /></code> is
    ["img"].*)
type tag = string

(** An [attribute] is a key-value pair of a DOM element. For example, in
    <code><img src="here.png" /></code>, there is an attribute of
    [("src", "here.png")].*)
type attribute = string * string

(** An [element] is an abstract representation of an element in a DOM
    tree.

    A [Text s] represents the raw text [s].

    A [Comment s] represents an XML comment containing [s].

    A [Item t atts] represents a DOM "item" -- that is, a DOM element
    with tag [t] and attributes [atts] which cannot contain other
    elements. For example, [Item "img" \[("src", "here.png")\]]
    corresponds to <code><img src="here.png" /></code> in XML.

    An [Container t atts elems] represents a DOM "container" -- that is,
    a DOM element with tag [t] and attributes [atts] which may contain
    other elements (i.e. has a closing tag). For example,
    [Container "div" \[\] \[Item "img" \[("src", "here.png")\]; Item
    "img" \[("src", "there.png")\]\]] corresponds to <code><div> <img
    src="here.png" /> <img src="there.png" /> </div></code>*)
type element =
  | Text of string
  | Comment of string
  | Item of tag * attribute list
  | Container of tag * attribute list * element list

(** [output_xml f ?tab_level:tl dom] writes the abstract DOM tree [dom]
    to the file [f] as XML with an indentation of [tl] tabs.*)
let rec output_xml f ?tab_level:(tl = 0) =
  let string_of_atts =
    List.fold_left
      (fun acc (k, v) -> Printf.sprintf "%s %s=\"%s\"" acc k v)
      ""
  in
  let tabs = String.make tl '\t' in
  function
  | Text s -> Printf.fprintf f "%s" s
  | Comment s -> Printf.fprintf f "%s<!--%s-->" tabs s
  | Item (elem, atts) ->
      Printf.fprintf f "%s<%s %s/>\n" tabs elem (string_of_atts atts)
  | Container (elem, atts, children) ->
      Printf.fprintf f "%s<%s %s>\n" tabs elem (string_of_atts atts);
      List.iter (output_xml ~tab_level:(tl + 1) f) children;
      Printf.fprintf f "%s</%s>\n" tabs elem
