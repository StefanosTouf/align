open Base

type match_result = Match | Pattern_End | No_Match [@@deriving ord, eq] 

type t = ((char -> match_result) array) * ((char -> match_result) array)

type direction = Forwards | Backwards

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; length  : int
                     ; idx     : int
                     }

(* let max_length = Array.length *)

(** Forwards  [[1;2;3]; [1]; [1;2]] -> [[1;2;3]; [1;empty;empty]; [1;2;empty]]
    Backwards [[1;2;3]; [1]; [1;2]] -> [[1;2;3]; [empty;empty;1]; [empty;1;2]] *)
let match_length arrs ~empty ~direction = 
  let max_len = 
    Array.map ~f:Array.length arrs |> Array.max_elt ~compare |> Option.value_exn
  in
  let pad arr = match direction with 
    | Forwards  -> Array.append arr (Array.init ~f:(fun _ -> empty) @@ max_len - Array.length arr)
    | Backwards -> Array.append (Array.init ~f:(fun _ -> empty) @@ max_len - Array.length arr) arr
  in
  Array.map ~f:pad arrs

let match_result_of_bool b = if b then Match else No_Match

let of_array arr = 
  let make direction = 
    Array.sorted_copy arr ~compare:(fun a a' -> compare_int (Array.length a') (Array.length a))
    |> Array.map ~f:(Array.map ~f:(fun c c' -> match_result_of_bool @@ equal_char c c'))
    |> match_length ~direction ~empty:(Fn.const Pattern_End)
    |> Array.transpose_exn 
    |> Array.map ~f:(fun arr c -> Array.map ~f:(fun pred -> pred c) arr |> Array.min_elt ~compare:compare_match_result |> Option.value_exn)
  in make Forwards, make Backwards

(* returns index of match along with length of match *)
let matchi ~direction ~offset ~str t =
  let (t_f, t_b) = t in
  let rec loop_forwards match_idx ofs idx match_length = 
    if      idx >= Array.length str && match_idx = Array.length t_f
    then    Some (idx - match_idx, match_length) 

    else if idx >= Array.length str
    then    None

    else if match_idx = Array.length t_f
    then 
      if   ofs = 0 
      then Some (idx - match_idx, match_length)
      else loop_forwards 0 (ofs - 1) (idx + 1) 0

    else if equal_match_result ((Array.get t_f match_idx) (Array.get str idx)) Match
    then    loop_forwards (match_idx + 1) ofs (idx + 1) (match_length + 1)

    else if equal_match_result ((Array.get t_f match_idx) (Array.get str idx)) Pattern_End
    then    loop_forwards (match_idx + 1) ofs (idx + 1) match_length

    else    loop_forwards 0 ofs (idx + 1) 0
  in 
  let rec loop_backwards match_idx ofs idx match_length =
    if      idx < 0 && match_idx < 0
    then    Some (idx + 1, match_length)

    else if idx < 0
    then    None

    else if match_idx < 0
    then    
      if   ofs = 0 
      then Some (idx + 1, match_length)
      else loop_backwards (Array.length t_b - 1) (ofs - 1) (idx - 1) 0

    else if equal_match_result ((Array.get t_b match_idx) (Array.get str idx)) Match
    then    loop_backwards (match_idx - 1) ofs (idx - 1) (match_length + 1)

    else if equal_match_result ((Array.get t_b match_idx) (Array.get str idx)) Pattern_End
    then    Some (idx + 1, match_length)

    else    loop_backwards (Array.length t_b - 1) ofs (idx - 1) 0

  in match direction with
  | Forwards  -> loop_forwards 0 offset 0 0
  | Backwards -> loop_backwards (Array.length t_b - 1) offset (Array.length str - 1) 0


let split_around t ~str ~offset ~direction =
  let open Option in
  matchi ~str ~offset ~direction t >>| fun (i, l) ->
    { before  = Array.sub str ~pos:0       ~len:i
    ; matched = Array.sub str ~pos:i       ~len:l
    ; after   = Array.sub str ~pos:(i + l) ~len:(Array.length str - i - l)
    ; idx     = i
    ; length  = l
    }

let t_of_sexp s = of_array @@ array_of_sexp (fun s -> String.to_array @@ string_of_sexp s) s
let pp _        = failwith "not used"
