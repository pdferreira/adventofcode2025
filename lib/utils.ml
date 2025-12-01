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
