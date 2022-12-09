open Base
open Util

module type Line = sig
  type t 

  val split_around : t -> char list * char * char list

  val position : t -> int

  val from_list : char list -> t option

  val to_list  : t -> char list

  val pad : t -> before:int -> after:int -> t
end

module Make_Line(M : sig val sym : char -> bool end) : Line = struct
  type t = char list * char * char list

  let position_of_sym l = let open Option in
    (List.findi ~f:(fun _ -> M.sym) l >>| fun (i, _) -> i)

  let position (h, _, _) = List.length h + 1 

  let split_around l =  l

  let from_list l = let open Option in 
   let* (b, a) = position_of_sym l >>| List.split_n l in
   Some (b, List.hd_exn a, List.drop a 1)

  let to_list (b, s, a) = List.concat [b ; [s]; a]

  let conform_leading_whites goal chars =
    let whites = List.take_while ~f:is_white chars |> List.length in
    let difference = whites - goal in
    if (difference > 0) then
      List.drop chars difference
    else 
      List.concat [ list_of_whites @@ neg difference; chars]

  let pad (b, s, a) ~before ~after =  
    let b' = List.rev b |> conform_leading_whites before |> List.rev
    and a' = conform_leading_whites after a
    in b', s, a'
end

