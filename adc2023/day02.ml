let input1 = "day02a-input.txt"

type set = {
  red : int;
  green : int;
  blue : int;
}

let string_to_set colours set_s =
  let cubes = String.split_on_char ',' set_s in
  let f num colour_s = match colour_s with
    | "blue" -> num <= colours.blue
    | "green" -> num <= colours.green
    | "red" -> num <= colours.red
    | s -> raise (Invalid_argument (Format.asprintf "Unknown: %s" s))
  in
  List.for_all (fun s -> Scanf.sscanf s " %u %s" f) cubes

let string_to_power set_s =
  let scan_set set_acc set_s =
    let bis set_acc colour_s =
      let num, colour = Scanf.sscanf colour_s " %u %s" (fun n c -> (n, c)) in
      begin match colour with
        | "blue" ->
          let new_blue = max set_acc.blue num in
          { red = set_acc.red; green = set_acc.green; blue = new_blue; }
        | "green" ->
          let new_green = max set_acc.green num in
          { red = set_acc.red; green = new_green; blue = set_acc.blue; }
        | "red" ->
          let new_red = max set_acc.red num in
          { red = new_red; green = set_acc.green; blue = set_acc.blue; }
        | s -> raise (Invalid_argument (Format.asprintf "Unknown: %s" s))
      end
    in
    List.fold_left bis set_acc (String.split_on_char ',' set_s)
  in
  let zero_set = { red = 0; green = 0; blue = 0; } in
  let min_set = List.fold_left scan_set zero_set set_s in
  min_set.red * min_set.green * min_set.blue

let parse_line line =
  let id_s, sets_s = match String.split_on_char ':' line with
    | [id_s; sets_s] -> id_s, sets_s
    | _ -> raise (Invalid_argument "Unknown")
  in
  let id = Scanf.sscanf id_s "Game %u" Fun.id in
  let strs = String.split_on_char ';' sets_s in
  id, strs

let line_to_res colours line =
  let id, strs = parse_line line in
  if List.for_all (fun s -> string_to_set colours s) strs then id else 0

let input_to_res colours input =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    let numbers = List.map (line_to_res colours) lines in
    List.fold_left (+) 0 numbers;
  with e ->
    close_in_noerr ic;
    raise e

let line_to_min_cubes line =
  let _, strs = parse_line line in
  string_to_power strs

let input_to_min_cubes input =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    let numbers = List.map line_to_min_cubes lines in
    List.fold_left (+) 0 numbers;
  with e ->
    close_in_noerr ic;
    raise e

let () =
  let res1 = input_to_res { red = 12; green = 13; blue = 14; } input1 in
  let res2 = input_to_min_cubes input1 in
  Printf.printf "%d\n%d\n" res1 res2
