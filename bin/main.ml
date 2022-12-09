open Base
open Util
open Line

let read_lines () =
  let stdin    = Stdio.In_channel.stdin      in 
  let read  () = Stdio.In_channel.input_lines stdin |> List.map ~f:(String.to_list)
  and close () = Stdio.In_channel.close stdin in
  Exn.protect ~f:read ~finally:close

module Formatter(M : Line) = struct
  let leading_whites line = 
    let (bef, _, _) = M.split_around line
    in
    List.rev bef |> List.take_while ~f:is_white |> List.length

  let cmp t t' = 
    compare (M.position t - leading_whites t) (M.position t' - leading_whites t')

  let align_with_leader l leader =
    let position   = M.position l 
    and (b, s, a)  = M.split_around l
    and leader_pos = M.position leader
    in
    let difference = leader_pos - position
    in
    if difference < 0 then
      List.concat [List.take b ((List.length b) + difference); [s]; a]
    else 
      List.concat [b; list_of_whites difference; [s]; a]

  let align lines ~before ~after =
    let ts = 
      List.map lines
        ~f:(fun t -> M.from_list t |> Option.map ~f:(M.pad ~before ~after)) 
    in
    let leader = 
      List.filter_opt ts
      |> List.max_elt ~compare:cmp
    in 
    let whites i line = match line, leader with
    | Some l, Some lead -> align_with_leader l lead
    |  _                -> List.nth_exn lines i
    in
      List.mapi ~f:whites ts
end

let run syms lines = 
 let module M = Make_Line(struct let sym = syms end) in
 let module F = Formatter(M) in
 F.align lines

let make_pred syms = 
  fun s -> List.exists ~f:(equal_char s) syms

let pipeline str chars = 
  List.fold 
  ~init:chars 
  ~f:(fun acc {symbols; before; after} -> run (make_pred symbols) acc ~before ~after) 
  (Transformation.parse_list str) 

let () = match (Sys.get_argv()).(1) with
 | s -> pipeline s (read_lines ()) |> List.map ~f:String.of_char_list |> String.concat ~sep:"\n" |> Stdio.print_endline

