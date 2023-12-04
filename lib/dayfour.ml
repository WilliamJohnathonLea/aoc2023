open Core

type card = {
  winners: int list;
  numbers: int list
}

let parse_card line =
  let filter_empties list = List.filter list ~f:(fun s -> not @@ String.is_empty s) in
  let card_numbers = List.last_exn (String.split line ~on:':') in
  let ws_and_ns = String.split card_numbers ~on:'|' in
  let ws = List.map (filter_empties @@ String.split (List.hd_exn ws_and_ns) ~on:' ') ~f:int_of_string in
  let ns = List.map (filter_empties @@ String.split (List.last_exn ws_and_ns) ~on:' ') ~f:int_of_string in
  {
    winners=ws;
    numbers=ns
  }

let filter_winners card =
  List.filter card.numbers ~f:(
    fun n -> List.exists card.winners ~f:(fun w -> n = w)
  )

let to_points winners =
  match winners with
  | [] -> 0
  | _ :: [] -> 1
  | _ :: tail ->
    List.fold_left (List.map tail ~f:(fun _ -> 2)) ~init:1 ~f:( * )

let part_one lines =
  let sum nums = List.fold_left nums ~init:0 ~f:(+) in
  let cards = List.map lines ~f:parse_card in
  let winners = List.map cards ~f:filter_winners in
  sum @@ List.map winners ~f:to_points

let part_two _ = 0
