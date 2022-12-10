open Base

type t

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; idx     : int
                     }

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val pp : Formatter.t -> t -> unit

val length : t -> int

val of_array : char array array  -> t

val of_single : char array  -> t

val matchi : t -> offset:int -> str:char array -> int option

val split_around : t -> str:char array -> offset:int -> deconstructed option
