open Base

let read_channel ch =
  let read  () = Stdio.In_channel.input_lines ch
  and close () = Stdio.In_channel.close ch in
  Exn.protect ~f:read ~finally:close

let read_lines = function
  | `Stdin  -> read_channel Stdio.In_channel.stdin 
  | `File f -> read_channel @@ Stdio.In_channel.create f 

let print_lines ls = String.concat ~sep:"\n" ls |> Stdio.print_endline

