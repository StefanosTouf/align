open Base
open Util

type t = { matcher   : Match.t
         ; offset    : int
         ; line      : char array
         ; left_pad  : int
         ; right_pad : int
         }

let is_not_white = Fn.compose not is_white 

let position { matcher; offset; line = str; left_pad; _} = 
  Match.matchi matcher ~offset ~str |> Option.value_exn |> (+) left_pad

let leading_whitespace {matcher; line = str; offset; _} =
  let open Match in
  let {before; _} = Match.split_around matcher ~str ~offset |> Option.value_exn in
  let rec count acc = function
    | idx when idx <= 0                  -> acc
    | idx when is_not_white before.(idx) -> acc
    | idx                                -> count (acc + 1) (idx - 1)
  in
  count 0 @@ Array.length before - 1

let trailing_whitespace {matcher; line = str; offset; _} =
  let open Match in
  let {after; _} = Match.split_around matcher ~str ~offset |> Option.value_exn in
  let rec count acc = function
    | idx when idx >= Array.length after -> acc
    | idx when is_not_white after.(idx)  -> acc
    | idx                                -> count (acc + 1) (idx + 1)
  in
  count 0 0

(* because of the use of matchi the rest of functions can use Option.value_exn *)
let from_chars ~matcher ~offset arr = 
  let open Option             in 
  Match.matchi ~str:arr ~offset matcher
  >>| Fn.const { matcher    
               ; offset
               ; line      = arr
               ; left_pad  = 0
               ; right_pad = 0
               }

let conform_before before difference =
  if (difference > 0) then
    Array.concat [before; array_of_whites difference]
  else 
    Array.sub before ~pos:0 ~len:(Array.length before + difference)

let conform_after after difference =
  if (difference > 0) then
    Array.concat [array_of_whites difference; after]
  else 
    Array.sub after ~pos:(neg difference) ~len:(Array.length after + difference)

let to_chars {matcher; left_pad; right_pad; line=str; offset} =
  let open Match in
  let {before; after; matched; _} = Match.split_around matcher ~str ~offset |> Option.value_exn in
  let conformed_b = conform_before before  left_pad 
  and conformed_a = conform_after  after   right_pad
  in
  Array.concat [conformed_b; matched; conformed_a]
  
 
(* pads are the difference between where a symbol is and where it should go *)
let pad t ~before ~after = 
  let {left_pad; right_pad; _} = t in
  { t with left_pad  = left_pad  + before
  ;        right_pad = right_pad + after
  }
