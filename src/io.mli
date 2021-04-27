(** Handles functionality relating to program IO, such as functions to
    write formatted output and helper functions for reading from files. *)

(** [print_header ?channel:c s] prints [s] to channel [c]. Bold,
    noticable styling is used if [c] is a virtual terminal. By default,
    [c=stdout].*)
val print_header : ?channel:out_channel -> string -> unit

(** [print_detail ?channel:c s] prints [s] to channel [c]. Less
    noticable styling is used if [c] is a virtual terminal. By default,
    [c=stdout].*)
val print_detail : ?channel:out_channel -> string -> unit

(** [print_error ?channel:c s] prints [s] to channel [c]. Bold,
    noticable, red styling is used if [c] is a virtual terminal. By
    default, [c=stderr].*)
val print_error : ?channel:out_channel -> string -> unit

(** [read_lines] returns a list of strings, where each entry is a line
    in [ch] (without the newline character). The list contains an entry
    for each line in [ch] and in *reverse* order. Requies: [ch] is
    readable.*)
val read_lines : in_channel -> string list
