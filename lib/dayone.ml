open Core

let word_digits = [
  "one", '1';
  "two", '2';
  "three", '3';
  "four", '4';
  "five", '5';
  "six", '6';
  "seven", '7';
  "eight", '8';
  "nine", '9';
  "1", '1';
  "2", '2';
  "3", '3';
  "4", '4';
  "5", '5';
  "6", '6';
  "7", '7';
  "8", '8';
  "9", '9';
]

let first_and_last_digit digits =
  match digits with
  | [] -> []
  | head :: [] -> [head; head]
  | head :: tail ->
    let last = List.hd_exn @@ List.rev tail in
    [head; last]

let digits_from_line line =
  let digits = List.filter (String.to_list line) ~f:(Char.is_digit) in
  let first_and_last = first_and_last_digit digits in
  int_of_string @@ String.of_list first_and_last


let get_indices line word digit =
  let idxs = String.substr_index_all line ~may_overlap:false ~pattern: word in
  List.map idxs ~f:(fun i -> digit, i)

let digits_from_line_pt2 line =
  let digits_and_idxs = List.fold_left ~f:(
    fun acc (word, d) ->
      acc @ get_indices line word d
  ) ~init:[] word_digits
  in
  let sorted = List.sort digits_and_idxs ~compare:(
    fun x y -> compare (snd x) (snd y)
  )
  in
  let nums = List.map sorted ~f:fst in
  int_of_string @@ String.of_list @@ first_and_last_digit nums
