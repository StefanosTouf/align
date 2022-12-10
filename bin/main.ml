open Base

let read_lines () =
  let stdin    = Stdio.In_channel.stdin      in 
  let read  () = Stdio.In_channel.input_lines stdin
  and close () = Stdio.In_channel.close stdin in
  Exn.protect ~f:read ~finally:close

let compare t t' = 
  compare (Align.position t  - Align.leading_whitespace t) 
          (Align.position t' - Align.leading_whitespace t')

let find_leader ts =
  List.filter_opt ts |> List.max_elt ~compare

let align_with_leader l leader =
  let difference = (Align.position leader) - (Align.position l)
  in Align.pad l 
    ~before:(difference - Align.leading_whitespace leader) 
    ~after:(neg @@ Align.trailing_whitespace l)

let align lines ~before ~after ~matcher ~offset =
  let ts = List.map lines ~f:(Align.from_string ~matcher ~offset) in
  match find_leader ts with 
  | None      -> lines
  | Some lead ->
    let whites (line, t) = match t with
      | Some l -> align_with_leader l lead |> Align.pad ~before ~after |> Align.to_string
      | None   -> line
    in 
      List.map ~f:whites (List.zip_exn lines ts)

let pipeline str chars = 
  let make_step matcher times before after = 
   Sequence.to_list @@ Sequence.unfold ~init:times ~f:(function
      | 0 -> None
      | n -> let make chars = align ~matcher ~offset:(times - n) ~before ~after chars
             in  Some (make, n - 1)
    )
  in 
  List.bind (Transformation.parse_list str) 
    ~f:(fun {symbols; word; before; after; times; _} -> match symbols, word with
       | Some s, None -> make_step [|Util.make_pred s|] times before after
       | None, Some w -> make_step (String.to_array w |> Array.map ~f:equal_char) times before after
       | _            -> failwith "AAAA"
       ) 
  |> List.fold ~init:chars ~f:(fun acc step -> step acc)

let () = pipeline (Sys.get_argv()).(1) (read_lines ()) 
         |> String.concat ~sep:"\n" 
         |> Stdio.print_endline

