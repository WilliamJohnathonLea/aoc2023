open Aoc2023.Dayone

let test_digits_from_line () =
  Alcotest.(check int) "should be 12" 12 (digits_from_line "1abc2")

let test_first_and_last_empty () =
  Alcotest.(check (list int)) "should be empty" [] (first_and_last_digit [])

let test_first_and_last_one_elem () =
  Alcotest.(check (list int)) "should have two elems" [1; 1] (first_and_last_digit [1])

let test_first_and_last_two_elems () =
  Alcotest.(check (list int)) "should have two elems" [1; 2] (first_and_last_digit [1; 2])

let test_first_and_last_three_elems () =
  Alcotest.(check (list int)) "should have two elems" [1; 3] (first_and_last_digit [1; 2; 3])

let test_digits_from_line_pt2 () =
  Alcotest.(check int) "should be 12" 12 (digits_from_line_pt2 "one3two")

let test_digits_from_line_pt2_overlap () =
  Alcotest.(check int) "should be 21" 21 (digits_from_line_pt2 "twone")

let test_test_digits_from_line_pt2_repeated_words () =
  Alcotest.(check int) "should be 66" 66 (digits_from_line_pt2 "sixthree8sixjxjqsjgjgp")

let test_test_digits_from_line_pt2_repeated_digits () =
  Alcotest.(check int) "should be 33" 33 (digits_from_line_pt2 "3one3")

let day_one_set = [
  "first_and_last on empty list", `Quick, test_first_and_last_empty;
  "first_and_last with a one element", `Quick, test_first_and_last_one_elem;
  "first_and_last with two elements", `Quick, test_first_and_last_two_elems;
  "first_and_last with three elements", `Quick, test_first_and_last_three_elems;
  "get digits '12' from line '1abc2'", `Quick, test_digits_from_line;
  "get words with index", `Quick, test_digits_from_line_pt2;
  "get words with index when overlapping", `Quick, test_digits_from_line_pt2_overlap;
  "get words with index with repeated words", `Quick, test_test_digits_from_line_pt2_repeated_words;
  "get words with index with repeated digits", `Quick, test_test_digits_from_line_pt2_repeated_digits;
]
