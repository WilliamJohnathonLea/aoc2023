open Aoc2023.Daytwo

let file = "daytwo.txt"
(* let file = "daytwo_test.txt" *)

let lines_from_file file =
  open_in file

let rec get_lines strings ch =
  match In_channel.input_line ch with
  | Some "done" -> strings
  | None -> strings
  | Some x -> get_lines (x :: strings) ch

let () =
  let config_hand = {red=12; green=13; blue=14} in
  let lines = get_lines [] (lines_from_file file) in
  let nums = List.map (
    fun l ->
      let game = parse_game l in
      if game_possible config_hand game then
        game.id
      else 0
  ) lines in

  let total = List.fold_left (+) 0 nums in
  print_endline "result total";
  print_endline @@ string_of_int total
