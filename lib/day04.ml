open List
open Utils

let neighbors grid x y =
  concat_map (fun dy ->
    filter_map (fun dx ->
      let nx = x + dx in
      let ny = y + dy in
      if dx = 0 && dy = 0 then
        None
      else if nx >= 0 && nx < Array.length grid.(0) && ny >= 0 && ny < Array.length grid then
        Some (grid.(ny).(nx))
      else
        None
    )
    [-1; 0; 1]
  ) [-1; 0; 1]

let is_accessible grid x y = (filter ((=) '@') (neighbors grid x y) |> length) < 4

let map_accessible_rolls f_accessible f_else grid =
  Array.mapi (fun y row -> 
    Array.mapi (fun x elem -> 
      if elem = '@' && is_accessible grid x y then
        f_accessible elem
      else
        f_else elem
    ) row
  ) grid

let count_accessible_rolls grid =
  grid
  |> map_accessible_rolls (fun _ -> 1) (fun _ -> 0)
  |> Array.fold_left (Array.fold_left (+)) 0

let clear_current_accessible_rolls grid =
  grid
  |> map_accessible_rolls (fun _ -> '.') (fun c -> c)

let count_rolls grid =
  grid
  |> Array.fold_left (Array.fold_left (fun acc c -> acc + (if c = '@' then 1 else 0))) 0

let clear_all_accessible_rolls grid =
  let orig_count = count_rolls grid in
  let final_grid = fixpoint clear_current_accessible_rolls grid in
  let final_count = count_rolls final_grid in
  orig_count - final_count

let solve_part f path =
  read_lines path
  |> map (fun l -> l |> String.to_seq |> Array.of_seq)
  |> Array.of_list
  |> f

let inputs = [
  "inputs/day04_example";
  "inputs/day04_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part count_accessible_rolls path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part clear_all_accessible_rolls path)) ;
)