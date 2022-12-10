open Base

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let is_white c = equal_char c ' ' 

let list_of_whites length =
  List.init ~f:(Fn.const ' ') length

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
