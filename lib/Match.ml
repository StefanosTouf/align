open Base

type t = (char -> bool) array

type direction = Forwards | Backwards

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; idx     : int
                     }

(** [[1;2;3]; [1]; [1;2]] -> [[1;2;3]; [1;empty;empty]; [1;2;empty]] *)
let match_length arrs ~empty = 
  let max_len = 
    Array.map ~f:Array.length arrs |> Array.max_elt ~compare |> Option.value_exn
  in
  let pad arr =
    Array.append arr (Array.init ~f:(fun _ -> empty) @@ max_len - Array.length arr)
  in
  Array.map ~f:pad arrs

let of_array arr = 
  Array.sorted_copy arr ~compare:(fun a a' -> compare_int (Array.length a') (Array.length a))
  |> Array.map ~f:(Array.map ~f:equal_char)
  |> match_length ~empty:(Fn.const true)
  |> Array.transpose_exn 
  |> Array.map ~f:(fun arr c -> Array.exists ~f:(fun pred -> pred c) arr)

let matchi ~direction ~offset ~str t =
  let rec loop_forwards match_idx ofs idx = 
    if      idx >= Array.length str && match_idx = (Array.length t)
    then    Some (idx - match_idx) 

    else if idx >= Array.length str
    then    None

    else if match_idx = Array.length t
    then 
      if   ofs = 0 
      then Some (idx - match_idx)
      else loop_forwards 0 (ofs - 1) (idx + 1)

    else if (Array.get t match_idx) (Array.get str idx)
    then    loop_forwards (match_idx + 1) ofs (idx + 1)

    else    loop_forwards 0 ofs (idx + 1)
  in 
  let rec loop_backwards match_idx ofs idx =
    if      idx < 0 && match_idx < 0
    then    Some (idx + 1)

    else if idx < 0
    then    None

    else if match_idx < 0
    then    
      if   ofs = 0 
      then Some (idx + 1)
      else loop_backwards (Array.length t - 1) (ofs - 1) (idx - 1)

    else if (Array.get t match_idx) (Array.get str idx)
    then    loop_backwards (match_idx - 1) ofs (idx - 1)

    else    loop_backwards (Array.length t - 1) ofs (idx - 1)

  in match direction with
  | Forwards  -> loop_forwards 0 offset 0
  | Backwards -> loop_backwards (Array.length t - 1) offset (Array.length str - 1)


let split_around t ~str ~offset ~direction =
  let open Option in
  matchi ~str ~offset ~direction t >>| fun i ->
    { before  = Array.sub str ~pos:0                    ~len:i
    ; matched = Array.sub str ~pos:i                    ~len:(Array.length t)
    ; after   = Array.sub str ~pos:(i + Array.length t) ~len:(Array.length str - i - Array.length t)
    ; idx     = i
    }

let t_of_sexp s = of_array @@ array_of_sexp (fun s -> String.to_array @@ string_of_sexp s) s
let pp _        = failwith "not used"



(* let%test_unit "index of word 1" = *)
(*   [%test_eq: int option] *) 
(*   (matchi ~offset:0 ([|equal_char 'a';equal_char 'b';equal_char 'c'|]) ~str:[| '3'; 'a'; 'b'; 'c'; '2'; '1' |]) *) 
(*   (Some 1) *) 

(* let%test_unit "index of word 2" = *)
(*   [%test_eq: int option] *) 
(*   (matchi ~offset:1 ([|equal_char 'a';equal_char 'b';equal_char 'c'|]) ~str:[| '3'; 'a'; 'b'; 'c'; '2'; 'a'; 'b'; 'c'; '1' |]) *) 
(*   (Some 5) *) 

(* let%test_unit "index of word 3" = *)
(*   [%test_eq: int option] *) 
(*   (matchi ~offset:0 ([|equal_char 'a';equal_char 'b';equal_char 'c'|]) ~str:[| '3'; 'a'; 'b'; '2'; 'c'; '1'; 'b'; 'c' |]) *) 
(*   (None) *) 

(* let%test_unit "index of word 4" = *)
(*   [%test_eq: int option] *) 
(*   (matchi ~offset:1 ([|equal_char 'a';equal_char 'b';equal_char 'c'|]) ~str:[| '3'; 'a'; 'b'; 'c'; '2'; 'c'; '1'; 'a'; 'b' ; 'c' |]) *) 
(*   (Some 7) *) 
