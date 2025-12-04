open List
open Utils

let rec find_largest_joltage ?(acc = 0) ~n_batteries bs =
  if n_batteries = 0 then
    acc
  else
    let max_d = fold_left max 0 (drop_right (n_batteries - 1) bs) in
    let max_idx = find_index ((=) max_d) bs |> Option.get in
    find_largest_joltage
      ~acc:(acc * 10 + max_d)
      ~n_batteries:(n_batteries - 1)
      (drop (max_idx + 1) bs)
;;

let solve_part path ~n_batteries =
  read_lines path
  |> map (fun s -> String.fold_right (fun c cs -> digit_to_int c :: cs) s [])
  |> map (find_largest_joltage ~n_batteries)
  |> fold_left (+) 0
;;

let inputs = [
  "inputs/day03_example";
  "inputs/day03_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part path ~n_batteries:2)) ;
  print_endline ("Part 1: " ^ string_of_int (solve_part path ~n_batteries:12)) ;
) 
;;