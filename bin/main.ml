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
 
