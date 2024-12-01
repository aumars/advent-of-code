let input = "day01_input.txt"

let calculate_distance list_a list_b =
  List.fold_left2 (fun acc a b -> acc + abs(a - b)) 0 list_a list_b

let convert_to_list lines =
  let rec bis lines (list_a, list_b) =
      match lines with
      | [] -> (list_a, list_b)
      | x :: rest ->
      let str_list = String.split_on_char ' ' x in
      let a = int_of_string (List.hd str_list) in
      let b = int_of_string (str_list |> List.rev |> List.hd) in
      bis rest (a :: list_a, b :: list_b)
    in
    bis lines ([], [])

let calculate_total_distance_between_lists list_a list_b =
  let list_sa = List.sort compare list_a in
  let list_sb = List.sort compare list_b in
  calculate_distance list_sa list_sb

let calculate_similarity_score list_a list_b =
  let tbl = Hashtbl.create 100 in
  let count_freq elem =
    if Hashtbl.mem tbl elem then
      let cur_freq = Hashtbl.find tbl elem in
      Hashtbl.replace tbl elem (cur_freq + 1)
    else
      Hashtbl.add tbl elem 1
  in
  List.iter (count_freq) list_b;
  let multiply_freq acc elem =
    if Hashtbl.mem tbl elem then
      (Hashtbl.find tbl elem) * elem + acc
    else
      acc
  in
  List.fold_left multiply_freq 0 list_a

let parse_input input =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    convert_to_list lines
  with e ->
    close_in_noerr ic;
    raise e

let () =
  let list_a, list_b = parse_input input in
  let total_distance = calculate_total_distance_between_lists list_a list_b in
  let similarity_score = calculate_similarity_score list_a list_b in
  Printf.printf "%d\n%d\n" total_distance similarity_score
