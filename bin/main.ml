open Base
open Line

let read_lines () =
  let stdin    = Stdio.In_channel.stdin      in 
  let read  () = Stdio.In_channel.input_lines stdin
  and close () = Stdio.In_channel.close stdin in
  Exn.protect ~f:read ~finally:close

module Formatter(M : Align) = struct
  let cmp t t' = 
    compare (M.position t - M.leading_whitespace t) (M.position t' - M.leading_whitespace t')

  let find_leader ts =
    List.filter_opt ts
    |> List.max_elt ~compare:cmp

  let align_with_leader l leader =
    let difference = (M.position leader) - (M.position l)
    in M.pad l 
      ~before:(difference - M.leading_whitespace leader) 
      ~after:(neg @@ M.trailing_whitespace l)

  let align lines ~before ~after =
    let ts = List.map lines ~f:M.from_string in
    match find_leader ts with 
    | None      -> lines
    | Some lead ->
      let whites (line, t) = match t with
        | Some l -> align_with_leader l lead |> M.pad ~before ~after |> M.to_string
        | None   -> line
      in 
        List.map ~f:whites (List.zip_exn lines ts)
end

let run syms lines ~offset = 
 let aline     = make_align syms offset in
 let module M  = (val aline)            in
 let module F  = Formatter(M)           in
 F.align lines

let pipeline str chars = 
  let make_step pred times before after = 
   Sequence.to_list @@ Sequence.unfold ~init:times ~f:(function
      | 0 -> None
      | n -> let make chars = run pred chars ~offset:(times - n) ~before ~after
             in  Some (make, n - 1)
    )
  in 
  List.bind (Transformation.parse_list str) 
    ~f:(fun {symbols; word; before; after; times; _} -> match symbols, word with
       | Some s, None -> make_step [|Util.make_pred s|] times before after
       | None, Some w -> make_step (String.to_array w |> Array.map ~f:equal_char) times before after
       | _            -> failwith "AAAA"
       ) 
  |> List.fold ~init:chars 
     ~f:(fun acc step -> step acc)

let () = pipeline (Sys.get_argv()).(1) (read_lines ()) 
         |> String.concat ~sep:"\n" 
         |> Stdio.print_endline

