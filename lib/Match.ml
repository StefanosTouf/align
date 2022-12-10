open Base

type t = char array array

type deconstructed = { before  : char array
                     ; matched : char array
                     ; after   : char array
                     ; idx     : int
                     }

let t_of_sexp = array_of_sexp (fun s -> String.to_array @@ string_of_sexp s)
let sexp_of_t = sexp_of_array (fun cs -> sexp_of_string @@ String.of_char_list @@ Array.to_list cs)
let pp _ = Fn.const ()

let length = Array.length

let same_length arrs = 
  let max_length = 
    Array.map ~f:(Array.length) arrs |> Array.max_elt ~compare |> Option.value_exn
  in
  let pad arr =
    let diff = max_length - Array.length arr in
    Array.append (Array.map ~f:(fun x -> Some x) arr) (Array.init ~f:(Fn.const None) diff )
  in
  Array.map ~f:pad arrs


let of_array = 
  Array.sorted_copy ~compare:(fun a a' -> compare_int (Array.length a') (Array.length a))

let of_single x = [|x|]

let matchi t ~offset ~str =
  (* Should contain an array a where every element is an array a' with 
     all characters that match a pattern in a position equal to the index of a in a' *)
  let pattern = 
    let find_if_some cs c = Array.find cs ~f:(function Some c' -> equal_char c c' | None -> true) in
    same_length t 
    |> Array.transpose_exn 
    |> Array.map ~f:(fun cs c -> Option.exists ~f:(Fn.const true) (find_if_some cs c)) 
  in
  Util.index_of_pattern ~offset ~pattern str


let split_around t ~str ~offset =
  let open Option in
  let ptr_length = 
    Array.map ~f:Array.length t |> Array.max_elt ~compare:compare_int |> Option.value_exn
  in
  matchi ~str ~offset t >>| fun i ->
    { before  = Array.sub str ~pos:0                ~len:i
    ; matched = Array.sub str ~pos:i                ~len:ptr_length
    ; after   = Array.sub str ~pos:(i + ptr_length) ~len:(Array.length str - i - ptr_length)
    ; idx     = i
    }

