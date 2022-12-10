open Base
open Util

module type Line = sig
  type t 

  val position : t -> int

  val leading_whitespace : t -> int

  val trailing_whitespace : t -> int

  val from_string : string -> t option

  val to_string  : t -> string

  val pad : t -> before:int -> after:int -> t
end

let make_line sym offset = (module struct 
  type t = { before    : char array
           ; after     : char array
           ; symbol    : char
           ; sym_index : int
           ; left_pad  : int
           ; right_pad : int
           }

  let is_not_white = Fn.compose not is_white 

  let position { sym_index; left_pad; _} = sym_index + left_pad

  let leading_whitespace { before; _} =
    let rec count acc = function
      | idx when idx <= 0                  -> acc
      | idx when is_not_white before.(idx) -> acc
      | idx                                -> count (acc + 1) (idx - 1)
    in
    count 0 @@ Array.length before - 1

  let trailing_whitespace { after; _} =
    let rec count acc = function
      | idx when idx >= Array.length after -> acc
      | idx when is_not_white after.(idx)  -> acc
      | idx                                -> count (acc + 1) (idx + 1)
    in
    count 0 0

  let from_string s = 
    let open Option             in 
    let arr = String.to_array s in
    Util.index_of arr ~offset ~pred:sym
    >>| fun i -> { before    = Array.sub arr ~pos:0 ~len:i 
                 ; after     = Array.sub arr ~pos:(i + 1) ~len:((Array.length arr) - (i + 1))
                 ; symbol    = Array.get arr i
                 ; sym_index = i
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

  let to_string t =
    let {before; after; symbol; left_pad; right_pad; _} = t in
    let conformed_b = conform_before before  left_pad 
    and conformed_a = conform_after  after   right_pad
    in
    String.of_char_list 
    @@ Array.to_list 
    @@ Array.concat [conformed_b; [|symbol|]; conformed_a]
    
   
  (* pads are the difference between where a symbol is and where it should go *)
  let pad t ~before ~after = 
    let {left_pad; right_pad; _} = t in
    { t with left_pad  = left_pad  + before
    ;        right_pad = right_pad + after
    }
end : Line)
