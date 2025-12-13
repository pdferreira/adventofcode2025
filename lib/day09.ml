open List
open Utils

let area (x1, y1) (x2, y2) = (1 + abs(x1 - x2)) * (1 + abs(y1 - y2))

let parse_tile s = match String.split_on_char ',' s with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith "not a 2D coord"

let solve_part1 path =
  let red_tiles = read_lines path |> map parse_tile in
  combinations red_tiles
    |> map (fun (t1, t2) -> area t1 t2)
    |> sort Int.compare
    |> rev
    |> hd

let solve path =
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  (* print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ; *)
