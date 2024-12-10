let r = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))\|do()\|don't()|}

let scan str = Scanf.sscanf str "mul(%d,%d)" (fun x y -> (x,y))

let compute_mul_instructions str cond =
  let rec find_matches pos (acc1, acc2) cond =
    try
      let new_pos = Str.search_forward r str pos in
      let match_str = Str.matched_string str in
      if String.compare match_str "do()" = 0 then
        find_matches (new_pos + String.length match_str) (acc1, acc2) true
      else if String.compare match_str "don't()" = 0 then
        find_matches (new_pos + String.length match_str) (acc1, acc2) false
      else
        let factor1, factor2 = scan match_str in
        if cond then
          find_matches (new_pos + String.length match_str) (acc1 + factor1 * factor2, acc2 + factor1 * factor2) cond
        else
          find_matches (new_pos + String.length match_str) (acc1 + factor1 * factor2, acc2) cond
    with Not_found ->
      acc1, acc2, cond
  in
  find_matches 0 (0, 0) cond

let parse_input input =
  let ic = open_in input in
  try
    let f (acc1, acc2, cond) s =
      let res1, res2, cond = compute_mul_instructions s cond in
      acc1 + res1, acc2 + res2, cond
    in
    let res1, res2, _ = In_channel.fold_lines f (0, 0, true) ic in
    close_in ic;
    res1, res2
  with e ->
    close_in_noerr ic;
    raise e

let () =
  if Array.length Sys.argv != 2 then
    raise (Invalid_argument "Puzzle input file required.")
  else
    let input_file = Sys.argv.(1) in
    let part1, part2 = parse_input input_file in
    Printf.printf "%d\n%d\n" part1 part2
