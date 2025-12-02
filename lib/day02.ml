open List
open Utils

type range = { min: int; max: int }
;;

let to_range range_s =
  match String.split_on_char '-' range_s with
  | [min; max] -> { min = int_of_string min; max = int_of_string max }
  | _ -> raise (Failure range_s)
;;

let rec is_invalid_id_part size rest =
  let rest_len = String.length rest in
  if rest_len >= 2 * size then
    let left_s = String.sub rest 0 size in
    let right_s = String.sub rest size size in
    left_s = right_s && is_invalid_id_part size (String.sub rest size (rest_len - size))
  else
    rest_len = size
;;

let is_invalid_id_v1 n =
  let s = string_of_int n in
  let s_len = String.length s in
  s_len mod 2 = 0 && is_invalid_id_part (s_len / 2) s
;;

let is_invalid_id_v2 n =
  let s = string_of_int n in
  let len = String.length s in
  let exception ReturnTrue in
  try
    for part_size = 1 to len / 2 do
      if len mod part_size = 0 && is_invalid_id_part part_size s then
        raise_notrace ReturnTrue 
    done ;
    false
  with ReturnTrue -> true
;;

let rec get_invalid_ids is_invalid min max =
  if min > max then
    []
  else if is_invalid min then
    min :: get_invalid_ids is_invalid (min + 1) max
  else
    get_invalid_ids is_invalid (min + 1) max
;;

let solve_part is_invalid path =
  read_lines path
  |> hd
  |> String.split_on_char ','
  |> map to_range
  |> concat_map (fun r -> get_invalid_ids is_invalid r.min r.max)
  |> fold_left (+) 0
;;

let inputs = [
  "inputs/day02_example";
  "inputs/day02_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part is_invalid_id_v1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part is_invalid_id_v2 path)) ;
) 
;;