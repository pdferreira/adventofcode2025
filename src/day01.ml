open In_channel
open List

let read_lines path =
  let raw_lines = with_open_text path input_lines in
  map String.trim raw_lines
;;

let skip_chars n s = String.sub s n (String.length s - n)
;;

let sign n =
  if n = 0 then 0
  else if n < 0 then -1
  else 1
;;

let int_of_bool b = if b then 1 else 0
;;

let to_number instr =
  let sign = if instr.[0] = 'L' then -1 else 1 in
  let n = int_of_string (skip_chars 1 instr) in
  sign * n
;;

let rotate n r = (n + r) mod 100
;;

let count_zero_stops start rs =
  rs
  |> fold_left (fun l r -> (rotate (hd l) r) :: l) [50]
  |> filter ((=) 0)
  |> length
;;

let count_zero_ticks_in_r start r =
  if start = 0 then
    if r = 0 then
      1
    else
      abs(r / 100)
  else
    let new_start = start + r in
    let sign_changed = int_of_bool (sign start != sign new_start) in
    sign_changed + abs(start + r) / 100
  
let count_zero_ticks start rs =
  rs
  |> fold_left
    (fun (s, t) r -> (rotate s r, t + count_zero_ticks_in_r s r))
    (start, 0)
  |> snd
;;

let solve_part1 path =
  read_lines path
  |> map to_number
  |> count_zero_stops 50
;;

let solve_part2 path =
  read_lines path
  |> map to_number
  |> count_zero_ticks 50
;;

[
  "inputs/day01_example";
  "inputs/day01_in"
] |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
) 
;;