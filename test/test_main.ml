open Test_dayone
open Test_daytwo

let () =
  Alcotest.run "test_aoc2023" [
    "day 1", day_one_set;
    "day 2", day_two_set;
  ]
