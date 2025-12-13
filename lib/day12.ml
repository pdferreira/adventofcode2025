open List
open Utils

type shape = Shape of (int * int) list

type region = Region of {
  width: int;
  height: int;
  shape_quantities: int list;
}

let rec repeat n f =
  if n <= 0 then
    []
  else
    f () :: repeat (n - 1) f

let parse ls =
  let rem_ls = Queue.of_seq (ls |> to_seq) in
  let shapes = ref [] in
  let regions = ref [] in
  while not (Queue.is_empty rem_ls) do
    let curr = Queue.take rem_ls in
    if String.ends_with ~suffix:":" curr then
      let shape_coords =
        repeat 3 (fun _ -> Queue.take rem_ls) 
        |> mapi (fun y l ->
          let add_coord (x, cs) c =
            if c = '#' then
              (x + 1, (x, y) :: cs)
            else
              (x + 1, cs)
          in
          String.fold_left add_coord (0, []) l
          |> snd
        )
        |> concat
      in
      let _ = Queue.take rem_ls in (* discard empty line *)
      shapes := Shape shape_coords :: !shapes 
    else
      let region =
        match String.split_on_char ':' curr with
          | [size_s; quantity_s] -> begin
            match String.split_on_char 'x' size_s with
            | [ws; hs] ->
              Region {
                width = int_of_string ws;
                height = int_of_string hs;
                shape_quantities =
                  String.split_on_char ' ' (String.trim quantity_s)
                  |> map int_of_string
                ;
              }
            | _ -> failwith ("invalid size_s: " ^ size_s)
          end
          | _ -> failwith ("invalid curr: " ^ curr)
      in
      regions := region :: !regions
  done;
  (Array.of_list !shapes, !regions)

let rec matrix_positions w h (x, y) =
  if x + 1 = w && y + 1 = h then
    Seq.singleton (x, y)
  else if x + 1 = w then
    Seq.cons (x, y) (matrix_positions w h (0, y + 1))
  else
    Seq.cons (x, y) (matrix_positions w h (x + 1, y))   

let rec place_shape w h matrix coords = match coords with
  | [] -> true
  | (x, y) :: coords' ->
    if x < 0 || x >= w || y < 0 || y >= h then
      false
    else if matrix.(y).(x) = true then
      false
    else begin
      matrix.(y).(x) <- true;
      place_shape w h matrix coords'
    end

let move_coord (dx, dy) (x, y) = (x + dx, y + dy)


let rotate_90_coord _ h (x, y) = (-y, x - h)

let rotate_180_coord w h (x, y) = (-x + w - 1, -y - h + 1)

let rotate_270_coord w _ (x, y) = (y + w - 1, -x)

let flip_horiz_coord w _ (x, y) = (w - 1 - x, y)

let flip_vert_coord _ h (x, y) = (x, -h + 1 - y)

let all_shape_variants w h coords =
  let base_variants = [
    coords;
    map (rotate_90_coord w h) coords;
    map (rotate_180_coord w h) coords;
    map (rotate_270_coord w h) coords;
  ] in
    base_variants
    @ map (map (flip_horiz_coord w h)) base_variants
    @ map (map (flip_vert_coord w h)) base_variants
    |> sort_uniq (List.compare (Pair.compare Int.compare Int.compare))


module CoordSet = Set.Make(struct
  type t = int * int
  let compare = Pair.compare Int.compare Int.compare
end)

let rec fits_presents w h matrix free_positions shapes = match shapes with
  | [] -> true
  | Shape coords :: shapes' ->
    let coord_variants = all_shape_variants w h coords in
    coord_variants |> exists (fun coord_variant ->
      free_positions |> CoordSet.exists (fun base_pos ->
        let m' = Array.map Array.copy matrix in
        let used_positions = (map (move_coord base_pos) coord_variant) in
        if place_shape w h m' used_positions then
          let free_positions' = CoordSet.union free_positions (CoordSet.of_list used_positions) in
          fits_presents w h m' free_positions' shapes'
        else
          false
      )
    )

let solve_part1 path =
  let shapes, regions = parse (read_lines path) in
  regions
  |> filter (fun (Region r) ->
    let matrix = Array.make_matrix r.height r.width false in
    let free_positions = matrix_positions r.width r.height (0, 0) |> CoordSet.of_seq in
    let req_shapes = r.shape_quantities |> mapi (fun i n -> repeat n (fun () -> shapes.(i))) |> concat in
    fits_presents r.width r.height matrix free_positions req_shapes
  )
  |> length

let solve path =
  print_endline ("[" ^ path ^ "]") ;
  print_endline ("Part 1: " ^ string_of_int (solve_part1 path));
  (* print_endline ("Part 2: " ^ string_of_int (solve_part2 path)); *)