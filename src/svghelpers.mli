(** Provides functions to help create SVG visual elements. *)

(** [make_text css_class attributes x y message] creates an SVG element
    which, when drawn, creates text containing [message] at position
    [x], [y], where [x] and [y] are valid CSS scalars (eg. "40", "12px",
    "50%"). It is assigned the stylesheet class [css_class].
    Additionally, the element inherits all of the XML attributes in
    [attributes]. *)
val make_text :
  string ->
  Xmldom.attribute list ->
  string ->
  string ->
  string ->
  Xmldom.element

(** [make_circle css_class attributes x y r] creates an SVG element
    which, when drawn, creates a circle of [r] at position [x], [y],
    where [x], [y], and [r] are valid CSS scalars (eg. "40", "12px",
    "50%"). It is assigned the stylesheet class [css_class].
    Additionally, the element inherits all of the XML attributes in
    [attributes]. *)
val make_circle :
  string ->
  Xmldom.attribute list ->
  string ->
  string ->
  string ->
  Xmldom.element

(** [make_polyline css_class attributes points] creates an SVG element
    which, when drawn, draws a line between each pair of sequential
    points in [points]. The coordinates of the points are relative to
    the SVG view containing the resulting element. It is assigned the
    stylesheet class [css_class]. Additionally, the element inherits all
    of the XML attributes in [attributes]. *)
val make_polyline :
  string -> Xmldom.attribute list -> Common.points -> Xmldom.element

(** [make_region_border css_class attributes top_left bottom_right]
    creates an SVG element which, when drawn, creates a border around
    the region specified by its coordinates [top_left] and
    [bottom_right]. The coordinates of the points are relative to the
    SVG view containing the resulting element. Each individual border
    line is assigned the stylesheet class [css_class] and, the grouping
    AS A WHOLE (not each individual border line) inherits all of the XML
    attributes in [attributes]. *)
val make_region_border :
  string ->
  Xmldom.attribute list ->
  Common.point ->
  Common.point ->
  Xmldom.element

(** [make_group attributes elems] creates a single group element
    containing all the elements in [elems]. The group element inherits
    the XML attributes in [attributes].*)
val make_group :
  Xmldom.attribute list -> Xmldom.element list -> Xmldom.element

(** [make_svg attributes elems] creates a single embedded SVG containing
    all the elements in [elems]. The SVG inherits the XML attributes in
    [attributes].*)
val make_svg :
  Xmldom.attribute list -> Xmldom.element list -> Xmldom.element
