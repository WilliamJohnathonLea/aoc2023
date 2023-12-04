open Aoc2023.Dayfour

let test_lines = [
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
]

let test_parse_card () =
  let expected = {
    winners=[41; 48; 83; 86; 17];
    numbers=[83; 86; 6; 31; 17; 9; 48; 53];
  } in
  Alcotest.(check (list int)) "test" expected.winners (parse_card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53").winners;
  Alcotest.(check (list int)) "test" expected.numbers (parse_card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53").numbers

let test_filter_winners () =
  let input = {
    winners=[41; 48; 83; 86; 17];
    numbers=[83; 86; 6; 31; 17; 9; 48; 53];
  } in
  let expected = [83; 86; 17; 48] in
  Alcotest.(check (list int)) "should be [83; 86; 17; 48]" expected (filter_winners input)

let test_to_points () =
  let input = [83; 86; 17; 48] in
  let expected = 8 in
  Alcotest.(check int) "test" expected (to_points input)

let test_part_one () =
  Alcotest.(check int) "test" 13 (part_one test_lines)


let day_four_set = [
  "test parsing a card", `Quick, test_parse_card;
  "test filtering the winning numbers on a card", `Quick, test_filter_winners;
  "test converting winners to points", `Quick, test_to_points;
  "test part one", `Quick, test_part_one;
]
