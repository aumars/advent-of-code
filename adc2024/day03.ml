let valid_mul_instruction = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let scan str = Scanf.sscanf str "mul(%d,%d)" (fun x y -> (x,y))

let compute_mul_instructions str =
  let rec find_matches pos acc =
    try
      let new_pos = Str.search_forward valid_mul_instruction str pos in
      let match_str = Str.matched_string str in
      let factor1, factor2 = scan match_str in
      find_matches (new_pos + String.length match_str) (acc + factor1 * factor2)
    with Not_found ->
      acc
  in
  find_matches 0 0

let parse_input input =
  let ic = open_in input in
  try
    let res = In_channel.fold_lines (fun acc s -> acc + compute_mul_instructions s) 0 ic in
    close_in ic;
    res
  with e ->
    close_in_noerr ic;
    raise e

let () =
  if Array.length Sys.argv != 2 then
    raise (Invalid_argument "Puzzle input file required.")
  else
    let input_file = Sys.argv.(1) in
    let part1 = parse_input input_file in
    Printf.printf "%d\n" part1
