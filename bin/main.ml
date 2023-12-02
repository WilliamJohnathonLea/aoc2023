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
  let games = List.map (parse_game) lines in
  let possible_games = List.filter (game_possible config_hand) games in
  let nums = List.map (
    fun game -> game.id
  ) possible_games in
  let powers = List.map (
    fun game -> power_of_hand @@ max_of_all_colours game.hands
  ) games in

  let total_ids = List.fold_left (+) 0 nums in
  let total_powers = List.fold_left (+) 0 powers in
  print_endline "id total";
  print_endline @@ string_of_int total_ids;
  print_endline "power total";
  print_endline @@ string_of_int total_powers;
