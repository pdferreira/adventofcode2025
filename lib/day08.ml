open List
open Utils


let connect b1 b2 circuits ~visited =
  let rev_index idx = length circuits - idx - 1 in

  match (CoordMap.find_opt b1 !visited, CoordMap.find_opt b2 !visited) with
    | Some b1_rev_idx, Some b2_rev_idx when b1_rev_idx = b2_rev_idx -> circuits
    | Some b1_rev_idx, Some b2_rev_idx ->
      let b1_idx = rev_index b1_rev_idx in
      
      let (b1_circuit, circuits') = remove b1_idx circuits |> Pair.map_fst Option.get in
      begin
        (* for circuits' and visited to be consistent, all boxes in circuits that 
            preceded the b1 circuit need to have their index adjusted (in reverse),
            as do all the boxes in the b1 circuit
        *)
        let b2_rev_idx' = if b2_rev_idx > b1_rev_idx then b2_rev_idx - 1 else b2_rev_idx in
        visited := !visited |> CoordMap.map (fun ri ->
          if ri > b1_rev_idx then
            ri - 1
          else if ri = b1_rev_idx then
            b2_rev_idx'
          else
            ri
        );

        let b2_idx' = rev_index b2_rev_idx' - 1 in (* circuits' has 1 less circuit *)
        update b2_idx' (CoordSet.union b1_circuit) circuits'
      end 
    | Some b1_rev_idx, None ->
      let b1_idx = rev_index b1_rev_idx in begin
        visited := CoordMap.add b2 b1_rev_idx !visited;
        circuits |> update b1_idx (CoordSet.add b2)
      end
    | None, Some b2_rev_idx ->
      let b2_idx = rev_index b2_rev_idx in begin
        visited := CoordMap.add b1 b2_rev_idx !visited;
        circuits |> update b2_idx (CoordSet.add b1)
      end
    | None, None -> 
      let rev_idx = length circuits in
      visited := CoordMap.add b1 rev_idx (CoordMap.add b2 rev_idx !visited);
      CoordSet.of_list [b1; b2] :: circuits

let solve_part1 path =
  let ls = read_lines path in
  let n = hd ls |> int_of_string in
  let bs = tl ls |> map parse_coord in
  let visited = (ref CoordMap.empty) in
  let circuits =
    combinations bs
    |> map (fun (b1, b2) -> (distance b1 b2, (b1, b2)))
    |> sort (Pair.compare Float.compare (Pair.compare coord_compare coord_compare))
    |> take n
    |> map snd
    |> fold_left (fun cs (b1, b2) -> connect b1 b2 cs ~visited) []
  in
  assert (length circuits >= 3) ; (* assumption: there's no need to consider the remaining *)
  circuits
  |> map CoordSet.cardinal
  |> sort Int.compare
  |> rev
  |> take 3
  |> fold_left ( * ) 1

let solve_part2 path =
  let ls = read_lines path in
  let bs = tl ls |> map parse_coord in
  let box_pairs_by_distance =
    combinations bs
    |> map (fun (b1, b2) -> (distance b1 b2, (b1, b2)))
    |> sort (Pair.compare Float.compare (Pair.compare coord_compare coord_compare))
    |> map snd
  in
  let visited = ref CoordMap.empty in
  let circuits = ref [] in
  let last_bp_opt = ref None in
  let box_pairs = ref box_pairs_by_distance in
  while
    not (is_empty !box_pairs)
    && not (
      length !circuits = 1
      && CoordMap.cardinal !visited = length bs
    )
  do
    let (b1, b2) = hd !box_pairs in
    circuits := connect b1 b2 !circuits ~visited;
    last_bp_opt := Some (b1, b2);
    box_pairs := tl !box_pairs
  done;
  let last_b1, last_b2 = Option.get !last_bp_opt in
  hd last_b1 * hd last_b2
  
let solve path =
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
