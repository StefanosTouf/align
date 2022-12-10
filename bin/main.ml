open Base
open Line
(* open Util *)

let read_lines () =
  let stdin    = Stdio.In_channel.stdin      in 
  let read  () = Stdio.In_channel.input_lines stdin
  and close () = Stdio.In_channel.close stdin in
  Exn.protect ~f:read ~finally:close

module Formatter(M : Line) = struct
  let cmp t t' = 
    compare (M.position t - M.leading_whitespace t) (M.position t' - M.leading_whitespace t')

  let align_with_leader l leader =
    let difference = (M.position leader) - (M.position l)
    in M.pad l 
      ~before:(difference - M.leading_whitespace leader) 
      ~after:(neg @@ M.trailing_whitespace l)

  let align lines ~before ~after =
    let ts = 
      List.map lines ~f:M.from_string
    in
    let leader = 
      List.filter_opt ts
      |> List.max_elt ~compare:cmp
    in 
    let whites i line = match line, leader with
    | Some l, Some lead -> align_with_leader l lead |> M.pad ~before ~after |> M.to_string
    |  _                -> List.nth_exn lines i
    in
      List.mapi ~f:whites ts
end

let run syms lines ~offset = 
 let line     = make_line syms offset in
 let module M = (val line)            in
 let module F = Formatter(M)          in
 F.align lines

let make_pred syms = 
  fun s -> List.exists ~f:(equal_char s) syms

let pipeline str chars = 
  let make_step pred times before after = 
   Sequence.to_list @@ Sequence.unfold ~init:times ~f:(function
      | 0 -> None
      | n -> let make chars = run pred chars ~offset:(times - n) ~before ~after
             in  Some (make, n - 1)
    )
  in 
  List.bind (Transformation.parse_list str) 
  ~f:(fun {symbols; before; after; times; _} -> make_step (make_pred symbols) times before after) 
  |> List.fold ~init:chars 
  ~f:(fun acc step -> step acc)

let () = pipeline (Sys.get_argv()).(1) (read_lines ()) 
         |> String.concat ~sep:"\n" 
         |> Stdio.print_endline

