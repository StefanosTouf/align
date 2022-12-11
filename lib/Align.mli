
type t 

(* where the symbol to be aligned is positioned in this Align *)
val position : t -> int

(* how many whitespace characters there are in this line before `position` *)
val leading_whitespace : t -> int

(* how many whitespace characters there are in this line after `position` *)
val trailing_whitespace : t -> int

val from_chars : matcher:Match.t -> offset:int -> char array -> t option

val to_chars  : t -> char array

(** the main operation of Align
    increases left pad by `before`
    and right pad by `after` *)
val pad : t -> before:int -> after:int -> t
