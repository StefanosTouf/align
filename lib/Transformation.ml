open Base

type t = { matcher : Match.t
         ; before  : int
         ; after   : int
         ; times   : int
         } [@@deriving sexp, show] 

let parse s = t_of_sexp @@ Parsexp.Single.parse_string_exn s

let parse_list s = String.split ~on:'|' s |> List.map ~f:parse
