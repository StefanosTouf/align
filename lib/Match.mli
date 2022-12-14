open Base

type t

type direction = Forwards | Backwards

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; length  : int
                     ; idx     : int
                     }

val t_of_sexp : Sexp.t -> t

val pp : Formatter.t -> t -> unit

val of_array : char array array  -> t

val split_around : t -> str:char array -> offset:int -> direction:direction -> deconstructed option
