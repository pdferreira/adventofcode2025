open List
open Utils

module StringMap = Map.Make(String)

module StringSet = Set.Make(String)

let parse_device_entry s = match String.split_on_char ':' s with
  | [d; outs] -> (d, String.split_on_char ' ' (String.trim outs))
  | _ -> failwith "invalid"

let rec count_paths m s e ~visited =
  if StringMap.mem s !visited then
    StringMap.find s !visited
  else
    if s = e then
      1
    else begin
      visited := StringMap.add s 0 !visited ;
      try
        StringMap.find s m
        |> map (fun n ->
          let total_n = count_paths m n e ~visited in
          visited := StringMap.add n total_n !visited;
          total_n
        )
        |> fold_left (+) 0
      with Not_found ->
        failwith ("Key not found: " ^ s)
    end

let solve_part1 path =
  read_lines path
  |> map parse_device_entry
  |> StringMap.of_list
  |> (fun m -> count_paths m "you" "out" ~visited:(ref StringMap.empty))

let solve_part2 path =
  let m =
    read_lines path
    |> map parse_device_entry
    |> StringMap.of_list
    |> StringMap.add "out" []
  in
  (* breakdown both possibilities *)
  let svr_dac_fft_out_total = [("svr", "dac"); ("dac", "fft"); ("fft", "out")]
    |> map (fun (s, e) -> count_paths m s e ~visited:(ref StringMap.empty))
    |> fold_left ( * ) 1
  in
  let svr_fft_dac_out_total = [("svr", "fft"); ("fft", "dac"); ("dac", "out")]
    |> map (fun (s, e) -> count_paths m s e ~visited:(ref StringMap.empty))
    |> fold_left ( * ) 1
  in
    svr_dac_fft_out_total + svr_fft_dac_out_total

let solve path =
  print_endline ("[" ^ path ^ "]") ;
  begin try
    print_endline ("Part 1: " ^ string_of_int (solve_part1 path))
  with Failure s ->
    print_endline ("Part 1: " ^ s);
  end;
  begin try
    print_endline ("Part 2: " ^ string_of_int (solve_part2 path))
  with Failure s ->
    print_endline ("Part 2: " ^ s);
  end;
