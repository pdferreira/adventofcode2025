open List
open Utils

module IntSet = Set.Make(Int)

let simulate_beams s in_beam_cols =
  IntSet.fold (fun c (n_splits, out_bs) ->
    if String.get s c = '^' then
      let out_bs' =
        if c = 0 then
          IntSet.add (c + 1) out_bs
        else if c + 1 = String.length s then
          IntSet.add (c - 1) out_bs
        else
          IntSet.add (c - 1) (IntSet.add (c + 1) out_bs)
      in
        (n_splits + 1, out_bs')
    else
      (n_splits, IntSet.add c out_bs)
  ) in_beam_cols (0, IntSet.empty)

let inc_assoc n key l = update_assoc ((+) n) key 0 l 

let simulate_quantum_beams s in_beam_cols =
  fold_left (fun out_bs (c, n_in_beams) ->
    if String.get s c = '^' then
      if c = 0 then
        inc_assoc n_in_beams (c + 1) out_bs
      else if c + 1 = String.length s then
        inc_assoc n_in_beams (c - 1) out_bs
      else
        inc_assoc n_in_beams (c - 1) (inc_assoc n_in_beams (c + 1) out_bs)
    else
      inc_assoc n_in_beams c out_bs
  ) [] in_beam_cols

let rec count_beam_splits ss beam_cols = match ss with
  | [] -> 0
  | s :: ss' ->
    let num_splits, beam_cols' = simulate_beams s beam_cols in
    num_splits + count_beam_splits ss' beam_cols'

let solve_part1 path =
  let ls = read_lines path in
  let start_col = String.index (hd ls) 'S' in
  count_beam_splits (tl ls) (IntSet.singleton start_col)

let solve_part2 path =
  let ls = read_lines path in
  let start_col = String.index (hd ls) 'S' in
  fold_left (fun acc s -> simulate_quantum_beams s acc) [(start_col, 1)] (tl ls)
  |> split
  |> snd
  |> fold_left (+) 0
  
let solve path =
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
