open Base

let read_lines () =
  let stdin    = Stdio.In_channel.stdin      in 
  let read  () = Stdio.In_channel.input_lines stdin
  and close () = Stdio.In_channel.close stdin in
  Exn.protect ~f:read ~finally:close

let find_leader ts =
  List.filter_opt ts |> List.max_elt ~compare:Line.compare

let align lines ~before ~after ~matcher ~offset =
  let ts = List.map lines ~f:(Line.from_chars ~matcher ~offset) in
  match find_leader ts with 
  | None        -> lines
  | Some leader ->
    let whites (line, t) = match t with
      | Some l -> Line.align_with l ~leader |> Line.pad ~before ~after |> Line.to_chars
      | None   -> line
    in 
      List.map ~f:whites (List.zip_exn lines ts)

let pipeline chars transformations = 
  let open Transformation in
  let make_step {matcher;times;before;after;_} = 
    let step = function 
      | 0 -> None
      | n -> Some (align ~matcher ~offset:(times - n) ~before ~after, n - 1)
    in
    Sequence.unfold ~init:times ~f:step |> Sequence.to_list
  in 
  List.bind ~f:make_step transformations 
  |> List.fold ~init:chars ~f:(|>)

let run lines = function
  | _ :: conf :: c :: n -> 
    let open Option in let open Transformation in
    let conf  = Transformation.config_of_sexp conf in
    let transformation = 
      let multiply n t = let {times; _} = t in { t with times = times * n } in
      let ts = List.Assoc.find ~equal:equal_string conf c |> Option.value_exn in
      (List.hd n >>| Int.of_string >>| fun n -> List.map ts ~f:(multiply n))
      |> Option.value ~default:ts
    in
      pipeline (List.map ~f:(String.to_array) lines) transformation
      |> List.map ~f:(fun x -> String.of_char_list @@ Array.to_list x)
      
  | _ -> failwith "invalid arguments"
 
let () = 
  let open Or_error in 
  begin try_with read_lines >>| fun lines ->
    match try_with (fun () -> run lines (Array.to_list (Sys.get_argv()))) with
    | Ok str  -> String.concat ~sep:"\n" str   |> Stdio.print_endline
    | Error _ -> String.concat ~sep:"\n" lines |> Stdio.print_endline
  end |> ignore
