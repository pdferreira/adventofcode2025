open List
open Utils

module IntSet = Set.Make(Int)

type button = IntSet.t

type light = bool

type jolt = int

type manual = {
  buttons: button list;
  target_lights: light array;
  target_joltage: jolt array;
}

let parse_light = function
  | '.' -> false
  | '#' -> true
  | _ -> failwith "unexpected"

let parse_lights s =
  let inner_s = String.sub s 1 (String.length s - 2) in
  String.fold_right (fun c l -> parse_light c :: l) inner_s [] |> Array.of_list

let parse_button s =
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ','
  |> map int_of_string
  |> IntSet.of_list


let parse_joltage s =
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ','
  |> map int_of_string
  |> Array.of_list

let parse_manual s =
  let parts = String.split_on_char ' ' s in
  {
    target_lights = hd parts |> parse_lights;
    buttons = tl parts |> drop_right 1 |> map parse_button;
    target_joltage = hd (rev parts) |> parse_joltage
  }

type search_state = {
  button_presses_rev: button list;
  curr_lights: light array;
  curr_joltage: jolt array;
}

module SearchStateOrd: Set.OrderedType with type t = search_state = struct
  type t = search_state
  let compare ss1 ss2 =
    if ss1 = ss2 then
      0
    else
      let cmp = Pair.compare
        Int.compare
        (Pair.compare (Array.compare Bool.compare) (Array.compare Int.compare))
      in
      let get_key ss = (length ss.button_presses_rev, (ss.curr_lights, ss.curr_joltage)) in
      cmp (get_key ss1) (get_key ss2)
end

module SearchStateSet = Set.Make(SearchStateOrd)

let press_button_l lights b =  b |> IntSet.iter (fun i -> lights.(i) <- not lights.(i))

let next_state_l ss b =
  let next_lights = Array.copy ss.curr_lights in
  press_button_l next_lights b ;
  {
    button_presses_rev = b :: ss.button_presses_rev;
    curr_lights = next_lights;
    curr_joltage = ss.curr_joltage; (* ignore *)
  }

let press_button_j jolts b = b |> IntSet.iter (fun i -> jolts.(i) <- jolts.(i) + 1)

let next_state_j ss b =
  let next_joltage = Array.copy ss.curr_joltage in
  press_button_j next_joltage b ;
  {
    button_presses_rev = b :: ss.button_presses_rev;
    curr_lights = ss.curr_lights; (* ignore *)
    curr_joltage = next_joltage;
  }

let reached_goal_l m curr_state = curr_state.curr_lights = m.target_lights

let reached_goal_j m curr_state = curr_state.curr_joltage = m.target_joltage

let is_goal_reachable_l (_: manual) (_: search_state) = true

let is_goal_reachable_j m curr_state = Array.for_all2 (>=) m.target_joltage curr_state.curr_joltage

let find_min_button_presses m ~next_state ~reached_goal ~is_goal_reachable =
  let to_visit = Queue.create () in
  let visited = ref SearchStateSet.empty in
  let min_state = ref None in
  Queue.add {
    button_presses_rev = [];
    curr_lights = Array.make (Array.length m.target_lights) false;
    curr_joltage = Array.make (Array.length m.target_joltage) 0;
  } to_visit;
  while not (Queue.is_empty to_visit) && Option.is_none !min_state do
    let curr_state = Queue.take to_visit in
    if SearchStateSet.mem curr_state !visited then
      ()
    else begin
      visited := SearchStateSet.add curr_state !visited;
      if reached_goal m curr_state then
        min_state := Some curr_state
      else if is_goal_reachable m curr_state then
        m.buttons |> iter (fun b ->
          Queue.add (next_state curr_state b) to_visit
        )
    end
  done;
  rev (Option.get !min_state).button_presses_rev 

let solve_part ~next_state ~reached_goal ~is_goal_reachable path =
  read_lines path
  |> map parse_manual
  |> map (find_min_button_presses ~next_state ~reached_goal ~is_goal_reachable)
  |> map length
  |> fold_left (+) 0

let solve_part1 = solve_part
  ~next_state:next_state_l
  ~reached_goal:reached_goal_l
  ~is_goal_reachable:is_goal_reachable_l

let solve_part2 = solve_part
  ~next_state:next_state_j
  ~reached_goal:reached_goal_j
  ~is_goal_reachable:is_goal_reachable_j

let solve path =
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path)) ;
  print_endline ("Part 2: " ^ string_of_int (solve_part2 path)) ;
