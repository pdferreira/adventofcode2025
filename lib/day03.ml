open List
open Utils

let rec inits l = match l with
  | [] -> []
  | [_] -> []
  | x :: xs -> x :: inits xs
;;

let find_largest_joltage ns =
  let max_d = fold_left max 0 (inits ns) in
  let max_idx = find_index ((=) max_d) ns |> Option.get in
  let next_max_d = fold_left max 0 (drop (max_idx + 1) ns) in
  max_d * 10 + next_max_d
;;

let solve_part1 path =
  read_lines path
  |> map (fun s -> String.fold_right (fun c cs -> (Char.code c - Char.code '0') :: cs) s [])
  |> map find_largest_joltage
  |> fold_left (+) 0
;;

let inputs = [
  "inputs/day03_example";
  "inputs/day03_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
) 
;;