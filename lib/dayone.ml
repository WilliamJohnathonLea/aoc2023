let is_digit c =
  let char_code = Char.code c in
  char_code >= 48 && char_code <= 57

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
