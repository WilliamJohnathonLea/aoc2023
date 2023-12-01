open Aoc2023.Dayone

let file = "dayone.txt"
(* let file = "dayone_test_pt2.txt" *)

let lines_from_file file =
  open_in file

let rec get_lines strings ch =
  match In_channel.input_line ch with
  | Some "done" -> strings
  | None -> strings
  | Some x -> get_lines (x :: strings) ch

let () =
  let lines = get_lines [] (lines_from_file file) in
  let nums = List.map digits_from_line_pt2 lines in

  List.iter2 (
    fun line num -> print_endline @@ line ^ " " ^ (string_of_int num)
  ) lines nums;

  let total = List.fold_left (+) 0 nums in
  print_endline "result total";
  print_endline @@ string_of_int total
