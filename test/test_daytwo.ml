open Aoc2023.Daytwo

let hand_pp ppf {red;green;blue} =
  Fmt.pf ppf "{red=%d; green=%d; blue=%d}" red green blue

let hand_equal x y = x = y

let hand = Alcotest.testable hand_pp hand_equal

let test_parse_game_no_hands () =
  let expected = {id=1; hands=[]} in
  Alcotest.(check int) "should be a game with ID 1" expected.id (parse_game "Game 1:").id;
  Alcotest.(check (list hand)) "should be empty" expected.hands (parse_game "Game 1:").hands

let test_parse_game_one_hand () =
  let expected = {id=1; hands=[{red=1;green=1;blue=1}]} in
  Alcotest.(check int) "should be a game with ID 1" expected.id (parse_game "Game 1:").id;
  Alcotest.(check (list hand)) "should have one element" expected.hands (parse_game "Game 1: 1 red, 1 green, 1 blue").hands

let test_parse_game_two_hands () =
  let expected = {
    id=1;
    hands=[
      {red=1;green=1;blue=1};
      {red=1;green=1;blue=0};
    ]
  } in
  Alcotest.(check int) "should be a game with ID 1" expected.id (parse_game "Game 1:").id;
  Alcotest.(check (list hand)) "should have one element" expected.hands (
    parse_game "Game 1: 1 red, 1 green, 1 blue; 1 red, 1 green"
  ).hands

let test_parse_hand_only_reds () =
  let input = parse_hand "2 red" in
  let expected = {red=2; blue=0; green=0} in
  Alcotest.(check int) "should be 2" expected.red input.red;
  Alcotest.(check int) "should be 0" expected.blue input.blue;
  Alcotest.(check int) "should be 0" expected.green input.green

let test_parse_hand_reds_blues () =
  let input = parse_hand "2 red, 1 blue" in
  let expected = {red=2; blue=1; green=0} in
  Alcotest.(check int) "should be 2" expected.red input.red;
  Alcotest.(check int) "should be 1" expected.blue input.blue;
  Alcotest.(check int) "should be 0" expected.green input.green

let test_parse_hand_reds_blues_greens () =
  let input = parse_hand "2 red, 1 blue, 3 green" in
  let expected = {red=2; blue=1; green=3} in
  Alcotest.(check int) "should be 2" expected.red input.red;
  Alcotest.(check int) "should be 1" expected.blue input.blue;
  Alcotest.(check int) "should be 3" expected.green input.green

let test_game_is_possible () =
  let config = {red=5;green=5;blue=5} in
  let input = game_possible config {id=1;hands=[{red=1;green=4;blue=5}]} in
  Alcotest.(check bool) "should be true" true input

let test_game_not_possible () =
  let config = {red=5;green=5;blue=5} in
  let input = game_possible config {id=1;hands=[{red=1;green=6;blue=5}]} in
  Alcotest.(check bool) "should be true" false input

let day_two_set = [
  "parse a game with ID 1 and no hands", `Quick, test_parse_game_no_hands;
  "parse a game with ID 1 and one hand", `Quick, test_parse_game_one_hand;
  "parse a game with ID 1 and two hands", `Quick, test_parse_game_two_hands;
  "parse a hand with only red cubes", `Quick, test_parse_hand_only_reds;
  "parse a hand with red and blue cubes", `Quick, test_parse_hand_reds_blues;
  "parse a hand with red, blue and green cubes", `Quick, test_parse_hand_reds_blues_greens;
  "a game is possible", `Quick, test_game_is_possible;
  "a game is not possible", `Quick, test_game_not_possible;
]
