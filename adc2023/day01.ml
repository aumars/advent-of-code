let input1 = "day01a-input.txt"
let input2 = "day01b-input.txt"

let digits1 = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let digits2 = ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let digits = List.combine digits1 digits2

let rec first_digit_in_line simple s = match s with
  | "" -> raise (Invalid_argument "Line does not have a first digit!")
  | s -> let find_in_s (a, b) = match simple with
      | true -> String.starts_with ~prefix:a s
      | false -> (String.starts_with ~prefix:a s) || (String.starts_with ~prefix:b s)
    in
    let index_option = List.find_index find_in_s digits in
    if Option.is_some index_option then
      Option.get index_option
    else
      let n = String.length s in
      first_digit_in_line simple (String.sub s 1 (n-1))

let rec last_digit_in_line simple s = match s with
  | "" -> raise (Invalid_argument "Line does not have a last digit!")
  | s -> let find_in_s (a, b) = match simple with
      | true -> String.ends_with ~suffix:a s
      | false -> (String.ends_with ~suffix:a s) || (String.ends_with ~suffix:b s)
    in
    let index_option = List.find_index find_in_s digits in
    if Option.is_some index_option then
      Option.get index_option
    else
      let n = String.length s in
      last_digit_in_line simple (String.sub s 0 (n-1))

let convert_line_to_number simple s =
  let tens = first_digit_in_line simple s in
  let ones = last_digit_in_line simple s in
  tens * 10 + ones

let input_to_res simple input =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    let numbers = List.map (convert_line_to_number simple) lines in
    List.fold_left (+) 0 numbers;
  with e ->
    close_in_noerr ic;
    raise e

let () =
  let res1 = input_to_res true input1 in
  let res2 = input_to_res false input2 in
  Printf.printf "%d\n%d\n" res1 res2
