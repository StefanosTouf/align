open Base

type t = { transposed : char option array array
         ; max_len    : int
         }

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; idx     : int
                     }


let of_array arr = 
  let max_len = 
    Array.map ~f:Array.length arr |> Array.max_elt ~compare |> Option.value_exn
  in
  let transposed = 
    Array.sorted_copy arr ~compare:(fun a a' -> compare_int (Array.length a') (Array.length a))
    |> Util.match_length ~max_len |> Array.transpose_exn 
  in 
    {max_len; transposed} 

let matchi {transposed; _} ~offset ~str =
  let pattern = 
    let find_if_some cs c = Array.find cs ~f:(function Some c' -> equal_char c c' | None -> true) in
    Array.map transposed ~f:(fun cs c -> Option.exists ~f:(Fn.const true) (find_if_some cs c)) 
  in
  Util.index_of_pattern ~offset ~pattern str

let split_around t ~str ~offset =
  let open Option in
  let {max_len; _} = t in
  matchi ~str ~offset t >>| fun i ->
    { before  = Array.sub str ~pos:0             ~len:i
    ; matched = Array.sub str ~pos:i             ~len:max_len
    ; after   = Array.sub str ~pos:(i + max_len) ~len:(Array.length str - i - max_len)
    ; idx     = i
    }

let t_of_sexp s = of_array @@ array_of_sexp (fun s -> String.to_array @@ string_of_sexp s) s
let sexp_of_t _ = failwith "not used"
let pp _        = failwith "not used"
