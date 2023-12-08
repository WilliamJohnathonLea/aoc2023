open Aoc2023.Dayseven

let file = "dayseven_test.txt"

let lines_from_file file =
  open_in file

let rec get_lines strings ch =
  match In_channel.input_line ch with
  | Some "done" -> strings
  | None -> strings
  | Some x -> get_lines (x :: strings) ch

let () =
  let lines = get_lines [] (lines_from_file file) in
  print_endline "part one";
  print_endline @@ string_of_int @@ part_one lines;
  print_endline "part two";
  print_endline @@ string_of_int @@ part_two lines;
