
type t 

(* where the symbol to be aligned is positioned in this Align *)
val position : t -> int

(* how many whitespace characters there are in this line before `position` *)
val leading_whitespace : t -> int

(* how many whitespace characters there are in this line after `position` *)
val trailing_whitespace : t -> int

val from_string : matcher:(char -> bool) array -> offset:int -> string -> t option

val to_string  : t -> string

(** the main operation of Align
    places `before` whitespaces before `position`
    and `after` whitespaces after `position` *)
val pad : t -> before:int -> after:int -> t