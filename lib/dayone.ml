let word_digits = [
  "one", "1";
  "two", "2";
  "three", "3";
  "four", "4";
  "five", "5";
  "six", "6";
  "seven", "7";
  "eight", "8";
  "nine", "9"
]

let is_digit c =
  let char_code = Char.code c in
  char_code >= 48 && char_code <= 57

let replace_word str old_word new_word =
  let rec replace acc str =
    let len_str = String.length str in
    let len_old_word = String.length old_word in

    if len_str < len_old_word then
      List.rev (str :: acc)
    else
      let substring = String.sub str 0 len_old_word in
      if substring = old_word then
        replace (new_word :: acc) (String.sub str len_old_word (len_str - len_old_word))
      else
        replace (String.sub str 0 1 :: acc) (String.sub str 1 (len_str - 1))
  in
  String.concat "" (replace [] str)

let first_and_last_digit digits =
  match digits with
  | [] -> []
  | head :: [] -> [head; head]
  | head :: tail ->
    let last = List.hd @@ List.rev tail in
    [head; last]

let digits_from_line line =
  let digits = Seq.filter (is_digit) @@ String.to_seq line in
  let first_and_last = List.to_seq @@ first_and_last_digit @@ List.of_seq digits in
  int_of_string @@ String.of_seq first_and_last

let digits_from_line_pt2 line =
  let digit_string = List.fold_left (
    fun acc (word, digit) ->
      replace_word acc word digit
  ) line word_digits
  in
  digits_from_line digit_string
