open Base

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let is_white c = equal_char c ' ' 

let array_of_whites length =
  Array.init ~f:(Fn.const ' ') length

let index_of arr ~offset ~pred =
  let rec loop ofs = function 
    | idx when idx >= Array.length arr                     -> None
    | idx when pred (Array.get arr idx) && equal_int ofs 0 -> Some idx
    | idx when pred (Array.get arr idx)                    -> loop (ofs - 1) (idx + 1)
    | idx                                                  -> loop ofs (idx + 1)
  in
    loop offset 0

let index_of_pattern arr ~offset ~pattern =
  let rec loop match_idx ofs idx = 
    if      idx >= Array.length arr && equal_int match_idx (Array.length pattern)
    then    Some (idx - match_idx) 

    else if idx >= Array.length arr
    then    None

    else if equal_int match_idx (Array.length pattern)
    then 
        if equal_int ofs 0 
        then Some (idx - match_idx)
        else loop 0 (ofs - 1) (idx + 1)

    else if (Array.get pattern match_idx) (Array.get arr idx)
    then    loop (match_idx + 1) ofs (idx + 1)

    else    loop 0 ofs (idx + 1)
  in loop 0 offset 0

let make_pred syms = 
  fun s -> List.exists ~f:(equal_char s) syms

let%test_unit "index of word 1" =
  [%test_eq: int option] 
  (index_of_pattern ~offset:0 ~pattern:([|equal_char 'a';equal_char 'b';equal_char 'c'|]) [| '3'; 'a'; 'b'; 'c'; '2'; '1' |]) 
  (Some 1) 

let%test_unit "index of word 2" =
  [%test_eq: int option] 
  (index_of_pattern ~offset:1 ~pattern:([|equal_char 'a';equal_char 'b';equal_char 'c'|]) [| '3'; 'a'; 'b'; 'c'; '2'; 'a'; 'b'; 'c'; '1' |]) 
  (Some 5) 

let%test_unit "index of word 3" =
  [%test_eq: int option] 
  (index_of_pattern ~offset:0 ~pattern:([|equal_char 'a';equal_char 'b';equal_char 'c'|]) [| '3'; 'a'; 'b'; '2'; 'c'; '1'; 'b'; 'c' |]) 
  (None) 

let%test_unit "index of word 4" =
  [%test_eq: int option] 
  (index_of_pattern ~offset:1 ~pattern:([|equal_char 'a';equal_char 'b';equal_char 'c'|]) [| '3'; 'a'; 'b'; 'c'; '2'; 'c'; '1'; 'a'; 'b' ; 'c' |]) 
  (Some 7) 
