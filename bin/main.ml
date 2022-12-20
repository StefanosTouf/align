open Base


let align lines ~before ~after ~matcher ~offset ~direction =
  let ts_opt     = List.map lines ~f:(Line.from_chars ~matcher ~offset ~direction) in
  let ts         = List.filter_opt ts_opt in
  let leader     = List.max_elt ~compare:Line.compare ts in
  let max_length = List.map ~f:Line.symbol_length ts |> List.max_elt ~compare:compare in
  match leader, max_length with 
  | Some leader, Some max_length ->
    let whites (line, t) = match t with
      | Some l -> Line.align_with l ~leader 
                  |> Line.pad ~before ~after:(max_length - Line.symbol_length l + after) 
                  |> Line.to_chars
      | None   -> line
    in 
      List.map ~f:whites @@ List.zip_exn lines ts_opt

  | _ -> lines

let pipeline transformations direction = 
  let open Config in let open List in
  let make_step {matcher;times;before;after;_} = 
    let step = function 
      | 0 -> None
      | n -> Some (align ~matcher ~offset:(times - n) ~before ~after ~direction, n - 1)
    in
    Sequence.unfold ~init:times ~f:step |> Sequence.to_list
  in 
  transformations >>= make_step 

let run Config.({transformations;multiplier;direction;lines}) =
  let transformations = 
    let multiply t = Config.({ t with times = t.times * multiplier }) in
    List.map ~f:multiply transformations
  in
  pipeline transformations direction 
  |> begin match direction with 
     | Match.Forwards  -> Fn.id
     | Match.Backwards -> List.rev
     end
  |> List.fold ~init:lines ~f:(|>)
  |> List.map ~f:(Fn.compose String.of_char_list Array.to_list)

let () = 
  match Config.get_config () with
  | Second conf  -> run conf |> IO.print_lines
  | First  lines -> IO.print_lines lines
 
