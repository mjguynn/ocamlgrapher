val make_text :
  string ->
  Xmldom.attribute list ->
  string ->
  string ->
  string ->
  Xmldom.element

val make_circle :
  string ->
  Xmldom.attribute list ->
  string ->
  string ->
  string ->
  Xmldom.element

val make_polyline :
  string -> Xmldom.attribute list -> Common.points -> Xmldom.element

val make_region_border :
  string ->
  Xmldom.attribute list ->
  Common.point ->
  Common.point ->
  Xmldom.element
