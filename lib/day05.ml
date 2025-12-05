open List
open Utils

let parse_range s = match String.split_on_char '-' s with
  | [start_s; end_s] -> (int_of_string start_s, int_of_string end_s)
  | _ -> raise (Failure s)

let is_fresh fresh_rs id = exists (fun r -> id >= fst r && id <= snd r) fresh_rs

let solve_part1 path =
  let ls = read_lines path in
  let fresh_rs = ls |> take_while ((<>) "") |> map parse_range in
  let stock_ids = ls |> drop (1 + length fresh_rs) |> map int_of_string in
  filter (is_fresh fresh_rs) stock_ids
  |> length

let rec count_ids ?(max=0) rs = match rs with
  | [] -> 0
  | (s, e) :: rs ->
    let real_s = Int.max s max in
    let (total, new_max) = if e < max then (0, max) else (e - real_s + 1, e + 1) in
    total + count_ids ~max:new_max rs

let solve_part2 path =
  let ls = read_lines path in
  let fresh_rs = ls |> take_while ((<>) "") |> map parse_range in
  let sorted_rs = sort (Pair.compare Int.compare Int.compare) fresh_rs in
  count_ids sorted_rs

let inputs = [
  "inputs/day05_example";
  "inputs/day05_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
)