open Core

exception Invalid_card

type card =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
[@@deriving ord, eq, show]

type hand = card list [@@deriving show]

type hand_and_bid = hand * int [@@deriving show]

type kind =
  | Five_of
  | Four_of
  | Full_house
  | Three_of
  | Two_pairs
  | One_pair
  | High_card
[@@deriving ord, eq, show]

let card_of_char c =
  match c with
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jack
  | 'T' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | _ -> raise Invalid_card

let or_else op1 op2 =
  match (op1, op2) with
  | (Some x, _) -> Some x
  | (None, Some x) -> Some x
  | _ -> None

let hand_of_string str =
  List.map (String.to_list str) ~f:card_of_char

let n_of_kind grouped_hand n kind =
  match List.find grouped_hand ~f:(fun cs -> List.length cs = n) with
  | None -> None
  | _ -> Some kind

let five_of_kind grouped_hand =
  n_of_kind grouped_hand 5 Five_of

let four_of_kind grouped_hand =
  n_of_kind grouped_hand 4 Four_of

let full_house grouped_hand =
  match List.find grouped_hand ~f:(
    fun cs ->
      (List.length cs = 3) &&
      (not @@ List.exists grouped_hand ~f:(fun l -> List.length l = 2))
  ) with
  | None -> None
  | _ -> Some Full_house

let three_of_kind grouped_hand =
  n_of_kind grouped_hand 3 Three_of

let two_pairs grouped_hand =
  let pairs = List.filter grouped_hand ~f:(fun h -> List.length h = 2) in
  if List.length pairs = 2
  then Some Two_pairs
  else None

let one_pair grouped_hand =
  let pairs = List.filter grouped_hand ~f:(fun h -> List.length h = 2) in
  if List.length pairs = 1
  then Some One_pair
  else None

let kind_of_hand hand =
  let grouped = List.sort_and_group hand ~compare:compare_card in
  let five_of = five_of_kind grouped in
  let four_of = four_of_kind grouped in
  let full_house = full_house grouped in
  let three_of = three_of_kind grouped in
  let two_pairs = two_pairs grouped in
  let one_pair = one_pair grouped in
  or_else five_of @@
  or_else four_of @@
  or_else full_house @@
  or_else three_of @@
  or_else two_pairs @@
  or_else one_pair (Some High_card)

let part_one lines =
  let hands_and_bids = List.map lines ~f:(fun l ->
    let split = String.split l ~on:' ' in
    let hand = hand_of_string @@ List.hd_exn split in
    let bid = int_of_string @@ List.last_exn split in
    hand, bid
  ) in
  List.iter hands_and_bids ~f:( fun (hand, _) ->
    match kind_of_hand hand with
    | Some k -> print_endline ((show_hand hand) ^ " " ^ (show_kind k))
    | None -> print_endline "no kind in hand"
  );
  0

let part_two _ = 0
