let () =
  Alcotest.run "test_aoc2023" [
    "day 1", Test_dayone.day_one_set;
    "day 2", Test_daytwo.day_two_set;
    (* "day 3", Test_daythree.day_three_set; *)
    "day 4", Test_dayfour.day_four_set;
  ]
