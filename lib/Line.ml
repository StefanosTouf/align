open Base

type t = { line      : Match.deconstructed
         ; leading   : int
         ; trailing  : int
         ; left_pad  : int
         ; right_pad : int
         }

let array_of_whites length =
  Array.init ~f:(Fn.const ' ') length

let is_not_white = Fn.compose not (equal_char ' ') 

let leading_whitespace before =
  let rec count acc idx = 
    if   idx <= 0 || is_not_white before.(idx) 
    then acc
    else count (acc + 1) (idx - 1)    
  in
  count 0 @@ Array.length before - 1

let trailing_whitespace after =
  let rec count acc idx = 
    if   idx >= Array.length after || is_not_white after.(idx)
    then acc 
    else count (acc + 1) (idx + 1)
  in
  count 0 0

let from_chars ~matcher ~offset ~direction arr = 
  let open Option in
  Match.split_around ~str:arr ~offset ~direction matcher
  >>| fun line -> { left_pad  = 0
                  ; right_pad = 0
                  ; leading   = leading_whitespace line.before
                  ; trailing  = trailing_whitespace line.after
                  ; line
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

(* applies pads to line *)
let to_chars {left_pad; right_pad; line={before; after; matched ;_}; _} =
  let conformed_b = conform_before before  left_pad 
  and conformed_a = conform_after  after   right_pad
  in  Array.concat [conformed_b; matched; conformed_a]
 
(* pads are the difference between where a symbol is and where it should go *)
let pad t ~before ~after = 
  let {left_pad; right_pad; _} = t in
  { t with left_pad  = left_pad  + before
  ;        right_pad = right_pad + after
  }

let compare t t' = 
  compare_int (t.line.idx  - t.leading  - t.left_pad ) 
              (t'.line.idx - t'.leading - t'.left_pad)

let align_with l ~leader =
  pad l ~before:(leader.line.idx - l.line.idx - leader.leading) ~after:(neg l.trailing)
