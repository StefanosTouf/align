open Base

type transformation = { matcher : Match.t
                      ; before  : int
                      ; after   : int
                      ; times   : int
                      } [@@deriving of_sexp, show] 

type t = { transformations : (string, transformation list) List.Assoc.t
         ; selector        : string
         ; multiplier      : int 
         ; direction       : Match.direction
         ; lines           : char array list
         }


let transformations_of_string s =
  List.Assoc.t_of_sexp string_of_sexp (list_of_sexp transformation_of_sexp) @@ Parsexp.Single.parse_string_exn s

let direction_of_string = function
  | "f" -> Match.Forwards
  | "b" -> Match.Backwards
  | d   -> failwith (String.concat ["Not a valid direction: "; d])

let get_or_else arr ~f ~index ~default = 
  try  f @@ Array.get arr index 
  with _ -> default

let multiplier_direction args =
  let aux i1 i2 = 
    let direction  = get_or_else args ~f:direction_of_string ~index:i1 ~default:Match.Forwards
    and multiplier = get_or_else args ~f:Int.of_string       ~index:i2 ~default:1
    in direction, multiplier
  in try       aux 3 4
     with _ -> aux 4 3

let get_config () = 
  let lines = IO.read_lines `Stdin in
  match Or_error.try_with @@ fun () ->
    let args = Sys.get_argv() in
    let direction, multiplier = multiplier_direction args 
    and transformations       = IO.read_lines (`File (Array.get args 1)) |> String.concat |> transformations_of_string
    and lines                 = List.map ~f:String.to_array lines
    and selector              = Array.get args 2
    in {transformations;direction;selector;multiplier;lines}
  with 
  | Ok t    -> Either.Second t
  | Error _ -> Either.First lines


