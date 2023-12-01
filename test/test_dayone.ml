open Aoc2023.Dayone

let test_is_digit_char_one () =
  Alcotest.(check bool) "should be true" true (is_digit '1')

let test_is_digit_char_a () =
  Alcotest.(check bool) "should be true" false (is_digit 'a')

let test_digits_from_line () =
  Alcotest.(check int) "should be 12" 12 (digits_from_line "1abc2")

let test_first_and_last_empty () =
  Alcotest.(check (list int)) "should be empty" [] (first_and_last_digit [])

let test_first_and_last_one_elem () =
  Alcotest.(check (list int)) "should have one elem" [1; 1] (first_and_last_digit [1])

let test_first_and_last_two_elems () =
  Alcotest.(check (list int)) "should have two elems" [1; 2] (first_and_last_digit [1; 2])

let test_first_and_last_three_elems () =
  Alcotest.(check (list int)) "should have two elems" [1; 3] (first_and_last_digit [1; 2; 3])

let day_one_set = [
  "'1' is a digit", `Quick, test_is_digit_char_one;
  "'a' is not a digit", `Quick, test_is_digit_char_a;
  "first_and_last on empty list", `Quick, test_first_and_last_empty;
  "first_and_last with a one element", `Quick, test_first_and_last_one_elem;
  "first_and_last with two elements", `Quick, test_first_and_last_two_elems;
  "first_and_last with three elements", `Quick, test_first_and_last_three_elems
]
