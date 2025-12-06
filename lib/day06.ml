open List
open Utils

let solve_part1 path =
  read_lines path
  |> map (fun s ->
      s
      |> String.trim
      |> String.split_on_char ' '
      |> filter ((<>) "")
  )
  |> transpose
  |> map (fun l -> match rev l with
    | [] -> raise (Failure "unexpected")
    | op :: ns ->
      let (op_f, base) = if op = "+" then ((+), 0) else (( * ), 1) in
      ns |> map int_of_string |> fold_left op_f base 
  )
  |> fold_left (+) 0

let solve_part2 path =
  read_lines path
  |> map (String.fold_left (fun acc c -> c :: acc) [])
  |> transpose
  |> split_on (for_all ((=) ' '))
  |> map rev
  |> map (fun nss -> match nss with
    | [] -> raise (Failure "")
    | ns_with_op :: nss -> match rev ns_with_op with
      | [] -> raise (Failure "")
      | op :: rev_ns ->
        let (op_f, base) = if op = '+' then ((+), 0) else (( * ), 1) in
        let parse_num ns' = ns' |> List.to_seq |> String.of_seq |> String.trim |> int_of_string in
        fold_left op_f base (map parse_num (rev rev_ns :: nss))
  )
  |> fold_left (+) 0
    
let inputs = [
  "inputs/day06_example";
  "inputs/day06_in"
]

let solve () = inputs |> iter (fun path ->
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
)