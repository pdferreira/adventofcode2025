open In_channel
open List

(** Strings **)

module CharSet = Set.Make(Char)

let trim excluded s =
  s
  |> String.to_seq |> List.of_seq
  |> List.drop_while (Fun.flip mem excluded)
  |> List.rev
  |> List.drop_while (Fun.flip mem excluded)
  |> List.rev
  |> List.to_seq
  |> String.of_seq

let skip_chars n s = String.sub s n (String.length s - n)

(** IO **)

let read_lines path =
  let raw_lines = with_open_text path input_lines in
  map (trim ['\n'; '\r']) raw_lines

(** Numbers **)

let int_of_digit c = Char.code c - Char.code '0'

let sign n =
  if n = 0 then 0
  else if n < 0 then -1
  else 1

let int_of_bool b = if b then 1 else 0

(** Lists **)

let rec drop_right n l = match l with
  | [] -> []
  | _ when length l <= n -> []
  | x :: xs -> x :: drop_right n xs


let rec transpose xss = match xss with
  | [] -> []
  | [xs] -> map singleton xs
  | xs :: xss -> combine xs (transpose xss) |> map (fun p -> fst p :: snd p)

let rec split_on f xs = match xs with
  | [] -> []
  | x :: xs ->
    let xs' = split_on f xs in
    if f x then
      [] :: xs'
    else match xs' with
      | [] -> [[x]]
      | x' :: xs' -> (x :: x') :: xs'

let rec update_assoc f key base l = match l with
  | [] -> [(key, f base)]
  | (k, v) :: l' when key = k -> (k, f v) :: l'
  | kv :: l' -> kv :: update_assoc f key base l'

let rec combinations l = match l with
  | [] -> []
  | x :: xs -> map (fun x' -> (x, x')) xs @ combinations xs

let remove i l =
  if i < 0 || i >= length l then
    failwith ("remove: Invalid index " ^ (string_of_int i) ^ " for list with length " ^ (string_of_int (length l)))
  else
    let rec inner_remove i = function
      | [] -> (None, [])
      | x :: xs ->
        if i = 0 then
          (Some x, xs)
        else
          inner_remove (i - 1) xs |> Pair.map_snd (cons x)
    in
    inner_remove i l

let update i f l =
  if i < 0 || i >= length l then
    failwith ("update: Invalid index " ^ (string_of_int i) ^ " for list with length " ^ (string_of_int (length l)))
  else
    let rec inner_update i = function
      | [] -> []
      | x :: xs ->
        if i = 0 then
          f x :: xs
        else
          x :: inner_update (i - 1) xs
    in
    inner_update i l

(** Functions **)

let rec fixpoint f b =
  let new_b = f b in
  if new_b = b then
    b
  else
    fixpoint f new_b

(** Coordinates **)

type coord = int list

let parse_coord s: coord = s |> String.split_on_char ',' |> map int_of_string

let coord_compare = List.compare Int.compare

let distance (b1: coord) (b2: coord) =
  map2 (fun v1 v2 -> float (v1 - v2) ** 2.) b1 b2
  |> fold_left (+.) 0.
  |> sqrt

module CoordOrd: Set.OrderedType with type t = coord = struct
  type t = coord
  let compare = coord_compare
end

module CoordSet = Set.Make(CoordOrd)

module CoordMap = Map.Make(CoordOrd)
