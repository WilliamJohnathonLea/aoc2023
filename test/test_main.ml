open Aoc2023.Myadd

let test_add_positives () =
  Alcotest.(check int) "should be 3" 3 (add 1 2)

let test_add_negatives () =
  Alcotest.(check int) "should be -3" (-3) (add (-1) (-2))

let test_add_pos_and_neg () =
  Alcotest.(check int) "should be -1" (-1) (add 1 (-2))

let test_set = [
  "add positives" ,`Quick, test_add_positives;
  "add negatives", `Quick, test_add_negatives;
  "add postive and negative", `Quick, test_add_pos_and_neg;
]

let () =
  Alcotest.run "test_add" [
    "add", test_set
  ]
