open List
open Utils

let box_compare = List.compare Int.compare

type box = int list

module BoxOrd: Set.OrderedType with type t = box = struct
  type t = int list
  let compare = box_compare
end

module BoxSet = Set.Make(BoxOrd)

module BoxMap = Map.Make(BoxOrd)

let parse_box s: box = s |> String.split_on_char ',' |> map int_of_string

let distance (b1: box) (b2: box) =
  map2 (fun v1 v2 -> float (v1 - v2) ** 2.) b1 b2
  |> fold_left (+.) 0.
  |> sqrt

let connect b1 b2 circuits ~visited =
  let rev_index idx = length circuits - idx - 1 in

  match (BoxMap.find_opt b1 !visited, BoxMap.find_opt b2 !visited) with
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
        visited := !visited |> BoxMap.map (fun ri ->
          if ri > b1_rev_idx then
            ri - 1
          else if ri = b1_rev_idx then
            b2_rev_idx'
          else
            ri
        );

        let b2_idx' = rev_index b2_rev_idx' - 1 in (* circuits' has 1 less circuit *)
        update b2_idx' (BoxSet.union b1_circuit) circuits'
      end 
    | Some b1_rev_idx, None ->
      let b1_idx = rev_index b1_rev_idx in begin
        visited := BoxMap.add b2 b1_rev_idx !visited;
        circuits |> update b1_idx (BoxSet.add b2)
      end
    | None, Some b2_rev_idx ->
      let b2_idx = rev_index b2_rev_idx in begin
        visited := BoxMap.add b1 b2_rev_idx !visited;
        circuits |> update b2_idx (BoxSet.add b1)
      end
    | None, None -> 
      let rev_idx = length circuits in
      visited := BoxMap.add b1 rev_idx (BoxMap.add b2 rev_idx !visited);
      BoxSet.of_list [b1; b2] :: circuits

let solve_part1 path =
  let ls = read_lines path in
  let n = hd ls |> int_of_string in
  let bs = tl ls |> map parse_box in
  let visited = (ref BoxMap.empty) in
  let circuits =
    combinations bs
    |> map (fun (b1, b2) -> (distance b1 b2, (b1, b2)))
    |> sort (Pair.compare Float.compare (Pair.compare box_compare box_compare))
    |> take n
    |> map snd
    |> fold_left (fun cs (b1, b2) -> connect b1 b2 cs ~visited) []
  in
  assert (length circuits >= 3) ; (* assumption: there's no need to consider the remaining *)
  circuits
  |> map BoxSet.cardinal
  |> sort Int.compare
  |> rev
  |> take 3
  |> fold_left ( * ) 1

let solve_part2 path =
  let ls = read_lines path in
  let bs = tl ls |> map parse_box in
  let box_pairs_by_distance =
    combinations bs
    |> map (fun (b1, b2) -> (distance b1 b2, (b1, b2)))
    |> sort (Pair.compare Float.compare (Pair.compare box_compare box_compare))
    |> map snd
  in
  let visited = ref BoxMap.empty in
  let circuits = ref [] in
  let last_bp_opt = ref None in
  let box_pairs = ref box_pairs_by_distance in
  while
    not (is_empty !box_pairs)
    && not (
      length !circuits = 1
      && BoxMap.cardinal !visited = length bs
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
