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

(** Functions **)

let rec fixpoint f b =
  let new_b = f b in
  if new_b = b then
    b
  else
    fixpoint f new_b