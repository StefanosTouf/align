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
