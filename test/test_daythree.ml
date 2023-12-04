open Aoc2023.Daythree

let example_input = [
  "467..114..";
  "...*......";
  "..35..633.";
  "......#...";
  "617*......";
  ".....+.58.";
  "..592.....";
  "......755.";
  "...$.*....";
  ".664.598..";
]

let test_part_one () =
  Alcotest.(check int) "should be 4361" 4361 (part_one example_input)

let day_three_set = [
  "sum of part one", `Quick, test_part_one;
]
