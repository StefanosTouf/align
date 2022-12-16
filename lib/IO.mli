open Base

val read_lines : [`Stdin | `File of string ] -> string list Or_error.t

val print_lines : string list -> unit
