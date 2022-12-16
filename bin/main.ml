open Base

let find_leader ts =
  List.filter_opt ts |> List.max_elt ~compare:Line.compare

let align lines ~before ~after ~matcher ~offset ~direction =
  let ts = List.map lines ~f:(Line.from_chars ~matcher ~offset ~direction) in
  match find_leader ts with 
  | None        -> lines
  | Some leader ->
    let whites (line, t) = match t with
      | Some l -> Line.align_with l ~leader |> Line.pad ~before ~after |> Line.to_chars
      | None   -> line
    in 
      List.map ~f:whites (List.zip_exn lines ts)

let pipeline transformations direction = 
  let open Config in
  let make_step {matcher;times;before;after;_} = 
    let step = function 
      | 0 -> None
      | n -> Some (align ~matcher ~offset:(times - n) ~before ~after ~direction, n - 1)
    in
    Sequence.unfold ~init:times ~f:step |> Sequence.to_list
  in 
  List.bind ~f:make_step transformations 

let run conf = let open Config in 
  let {transformations;selector;multiplier;direction;lines} = conf in
  let transformation = 
    let multiply transformation = 
      let {times; _} = transformation in { transformation with times = times * multiplier }
    in
    let ts = List.Assoc.find ~equal:equal_string transformations selector |> Option.value_exn in
    List.map ts ~f:multiply
  in
    pipeline transformation direction 
    |> begin match direction with 
       | Match.Forwards  -> Fn.id
       | Match.Backwards -> List.rev
       end
    |> List.fold ~init:lines ~f:(|>)
    |> List.map ~f:(fun x -> String.of_char_list @@ Array.to_list x)

let () = 
  match Config.get_config () with
  | Second conf  -> run conf |> IO.print_lines
  | First  lines -> IO.print_lines lines
 
(* let run lines = function *)
(*   | _ :: conf :: c :: n :: d :: _ -> *) 
(*     let conf  = Config.config_of_string conf in *)
(*     let transformation = *) 
(*       let multiply n t = let {Config.times; _} = t in { t with times = times * n } in *)
(*       let ts = List.Assoc.find ~equal:equal_string conf c |> Option.value_exn in *)
(*       Int.of_string n |> fun n -> List.map ts ~f:(multiply n) *)
(*     in *)
(*     let chars = (List.map ~f:(String.to_array) lines) in *)

(*     begin match d with *) 
(*     | "f" -> pipeline transformation `Forwards *)
(*     | "b" -> pipeline transformation `Backwards |> List.rev *)
(*     | _   -> failwith "invalid arguments" *)
(*     end *) 
(*     |> List.fold ~init:chars ~f:(|>) *)
(*     |> List.map ~f:(fun x -> String.of_char_list @@ Array.to_list x) *)
      
(*   | _ -> failwith "invalid arguments" *)
 
(* let () = *) 
(*   let lines = read_lines () in *)
(*   let str = run lines (Array.to_list (Sys.get_argv())) in *)
(*   String.concat ~sep:"\n" str   |> Stdio.print_endline *)

(* let () = *) 
(*   let open Or_error in *) 
(*   begin IO.read_lines `Stdin >>| fun lines -> *)
(*     match try_with (fun () -> run lines (Array.to_list (Sys.get_argv()))) with *)
(*     | Ok str  -> String.concat ~sep:"\n" str   |> Stdio.print_endline *)
(*     | Error e -> Stdio.print_endline @@ Error.to_string_hum e; String.concat ~sep:"\n" lines |> Stdio.print_endline *)
(*   end |> ignore *)
