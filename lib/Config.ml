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

let ( let* ) r f =
  match r with
  | Error e -> Error e
  | Ok x    -> f x

let ( and* ) r r' =
  match r, r' with
  | Error e, _        -> Error e
  | _      , Error e' -> Error e'
  | Ok x   , Ok x'    -> Result.Ok (x, x')

let transformations_of_string s = Or_error.try_with @@ fun () ->
  List.Assoc.t_of_sexp string_of_sexp (list_of_sexp transformation_of_sexp) @@ Parsexp.Single.parse_string_exn s

let direction_of_string = function
  | "f" -> Ok Match.Forwards
  | "b" -> Ok Match.Backwards
  | d   -> Or_error.error_string (String.concat ["Not a valid direction: "; d])

let get_config () = 
  let open Or_error in
  let lines = IO.read_lines `Stdin in
  let res = 
    let* args = try_with (fun () -> Array.to_list (Sys.get_argv())) in
    match args with 
    | _ :: conf_file :: selector :: n :: d :: _ -> 
      let* transformations = IO.read_lines (`File conf_file) >>| String.concat >>= transformations_of_string
      and* direction       = direction_of_string d
      and* multiplier      = try_with @@ fun () -> Int.of_string n
      and* lines           = lines >>| List.map ~f:String.to_array
      in Ok {transformations;direction;selector;multiplier;lines}

    | _ -> error_string "Invalid arguments"
  in
  match res with 
  | Ok t    -> Either.Second t
  | Error _ -> Either.First (lines |> Or_error.ok_exn)


