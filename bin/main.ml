open Aoc2023.Dayone

let rec get_lines strings =
  match In_channel.input_line In_channel.stdin with
  | Some "done" -> strings
  | None -> strings
  | Some x -> get_lines (x :: strings)

let () =
  let lines = get_lines [] in
  let nums = List.map digits_from_line_pt2 lines in
  let total = List.fold_left Int.add 0 nums in
  print_endline @@ string_of_int total
