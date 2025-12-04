open In_channel
open List

let read_lines path =
  let raw_lines = with_open_text path input_lines in
  map String.trim raw_lines
;;

let skip_chars n s = String.sub s n (String.length s - n)
;;

let sign n =
  if n = 0 then 0
  else if n < 0 then -1
  else 1
;;

let int_of_bool b = if b then 1 else 0
;;


let rec drop_right n l = match l with
  | [] -> []
  | _ when length l <= n -> []
  | x :: xs -> x :: drop_right n xs
;;

let digit_to_int c = Char.code c - Char.code '0'
;;