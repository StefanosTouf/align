open Base


type transformation = { matcher : Match.t
                      ; before  : int
                      ; after   : int
                      ; times   : int
                      } [@@deriving of_sexp, show] 

type t = { transformations : transformation list
         ; multiplier      : int 
         ; direction       : Match.direction
         ; lines           : char array list
         }

val get_config : unit -> (string list, t) Either.t
