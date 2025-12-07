module type DayModule = sig val solve: string -> unit end

let get_day (name: string): (module DayModule) = match name with
  | "day01" -> (module Lib.Day01)
  | "day02" -> (module Lib.Day02)
  | "day03" -> (module Lib.Day03)
  | "day04" -> (module Lib.Day04)
  | "day05" -> (module Lib.Day05)
  | "day06" -> (module Lib.Day06)
  | "day07" -> (module Lib.Day07)
  | _ -> failwith ("Unknown: " ^ name)

let input_dir = "inputs"

let () =
  if Array.length Sys.argv < 2 then begin
    invalid_arg "Missing day name"
  end ;
  let name = Sys.argv.(1) in
  let module D = (val get_day name) in
    Sys.readdir input_dir
    |> Array.to_seq
    |> Seq.filter (String.starts_with ~prefix:name)
    |> Seq.map (fun s -> input_dir ^ "/" ^ s)
    |> Seq.iter D.solve