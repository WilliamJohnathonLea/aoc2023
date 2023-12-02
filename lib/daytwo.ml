open Core

type hand = {
  red: int;
  green: int;
  blue: int
}

type game = {
  id: int;
  hands: hand list
}

let swap (a, b) = b, a

let parse_hand string =
  let tbl = Hashtbl.create (module String) in
  let cs = String.split string ~on:',' in
  let _ = List.map cs ~f:(
    fun c ->
      let trimmed = String.strip c in
      let (c, n) = swap @@ String.lsplit2_exn trimmed ~on:' ' in
      Hashtbl.add tbl ~key:c ~data:(int_of_string n)
  ) in
  {
    red=(Option.value (Hashtbl.find tbl "red") ~default:0);
    green=(Option.value (Hashtbl.find tbl "green") ~default:0);
    blue=(Option.value (Hashtbl.find tbl "blue") ~default:0)
  }

let parse_game line =
  let game_line = String.split line ~on:':' in

  let game_header = String.split (List.hd_exn game_line) ~on:' ' in
  let id = int_of_string (List.last_exn game_header) in

  let game_hands = List.last_exn game_line in

  let hands_unparsed = List.filter (
    String.split game_hands ~on:';'
  ) ~f:(fun s -> not (String.is_empty s)) in

  let hands = List.map hands_unparsed ~f:parse_hand in
  {id=id; hands=hands}

let hand_possible config_hand hand =
  (hand.red <= config_hand.red) &&
  (hand.green <= config_hand.green) &&
  (hand.blue <= config_hand.blue)

let game_possible config_hand game =
  List.for_all game.hands ~f:(hand_possible config_hand)

let power_of_hand hand =
  let red = if hand.red <= 0 then 1 else hand.red in
  let green = if hand.green <= 0 then 1 else hand.green in
  let blue = if hand.blue <= 0 then 1 else hand.blue in
  red * green * blue

let max_of_all_colours hand =
  let non_zero i = i > 0 in
  let reds =
    List.filter
    (List.map hand ~f:(fun h -> h.red))
    ~f:non_zero
  in
  let greens =
    List.filter
    (List.map hand ~f:(fun h -> h.green))
    ~f:non_zero
  in
  let blues =
    List.filter
    (List.map hand ~f:(fun h -> h.blue))
    ~f:non_zero
  in
  let r_min = List.max_elt reds ~compare:(Int.compare) in
  let g_min = List.max_elt greens ~compare:(Int.compare) in
  let b_min = List.max_elt blues ~compare:(Int.compare) in
  {
    red=Option.value ~default:1 r_min;
    green=Option.value ~default:1 g_min;
    blue=Option.value ~default:1 b_min
  }
