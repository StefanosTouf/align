open Base

type t = { matcher : Match.t
         ; before  : int
         ; after   : int
         ; times   : int
         } [@@deriving of_sexp, show] 

let parse s = t_of_sexp @@ Parsexp.Single.parse_string_exn s


let config_of_sexp s = List.Assoc.t_of_sexp string_of_sexp (list_of_sexp t_of_sexp) @@ Parsexp.Single.parse_string_exn s
