open Base

let read_channel ch =
  let read  () = Stdio.In_channel.input_lines ch
  and close () = Stdio.In_channel.close ch in
  Or_error.try_with @@ fun () -> Exn.protect ~f:read ~finally:close

let read_lines = function
  | `Stdin  -> read_channel Stdio.In_channel.stdin 
  | `File f -> let open Or_error in
      try_with (fun () -> Stdio.In_channel.create f) >>= read_channel

let print_lines ls = String.concat ~sep:"\n" ls |> Stdio.print_endline

