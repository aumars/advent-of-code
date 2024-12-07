let set_increasing report =
  match report with
  | first :: second :: _ -> first < second
  | _ -> raise (Invalid_argument "set_increasing: report does not have at least 2 elements")

let rec is_report_safe tolerance report =
  let rec bis l tolerance increasing =
    match l with
    | x :: y :: s ->
      let condition_check = ((increasing && x < y) || (not increasing && x > y)) && 1 <= abs(x - y) && abs(x-y) <= 3 in
      if condition_check then
        bis (y :: s) tolerance increasing
      else
        if tolerance then
          let rec f left right =
            match right with
            | x :: s ->
              let res = is_report_safe false ((List.rev left) @ s) in
              if res then
                true
              else
                f (x :: left) s
            | [] -> false
          in
          f [] report
        else
          false
    | _ :: [] -> true
    | [] -> raise (Invalid_argument "is_report_safe: report does not have at least 2 elements")
  in
  let increasing = set_increasing report in
  bis report tolerance increasing

let parse_report str =
 let str_list = String.split_on_char ' ' str in
 List.map int_of_string str_list

let parse_input input tolerance =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    let safe_reports = List.map (fun s -> parse_report s |> is_report_safe tolerance) lines in
    List.fold_left (fun acc elem -> if elem then acc + 1 else acc) 0 safe_reports
  with e ->
    close_in_noerr ic;
    raise e

let () =
  if Array.length Sys.argv != 2 then
    raise (Invalid_argument "Puzzle input file required.")
  else
    let input_file = Sys.argv.(1) in
    let part1 = parse_input input_file false in
    let part2 = parse_input input_file true in
    Printf.printf "%d\n%d\n" part1 part2
