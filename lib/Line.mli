
type t 

val from_chars : matcher:Match.t -> offset:int -> direction:Match.direction -> char array -> t option

val to_chars  : t -> char array

val pad : t -> before:int -> after:int -> t

(** The greater line is the one that is more aligned. 
    The more aligned has less left whitespace *)
val compare : t -> t -> int

(** pads t to align it with leader *)
val align_with : t -> leader:t -> t
