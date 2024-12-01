(* Function to check if a given position is within the bounds of the schematic *)
let is_valid_position x y rows cols =
  x >= 0 && x < rows && y >= 0 && y < cols

(* Function to check if a character is a symbol *)
let is_symbol c =
  c = '*' || c = '#' || c = '+' || c = '$'

(* Function to recursively calculate the sum of part numbers *)
let rec sum_of_part_numbers schematic x y =
  let rows = Array.length schematic in
  let cols = Array.length schematic.(0) in

  let is_symbol_adjacent (nx, ny) =
    is_valid_position nx ny rows cols && is_symbol schematic.(nx).(ny)
  in

  if is_valid_position x y rows cols then
    begin

      (* Check if the current position contains a part number *)
      let number =
        match schematic.(x).(y) with
        | '0' .. '9' -> true
        | _ -> false
      in

      let neighbors = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1); (x - 1, y - 1); (x - 1, y + 1); (x + 1, y - 1); (x + 1, y + 1)] in

      let evaluate_part_number acc (nx, ny) =
        if List.exists is_symbol_adjacent neighbors then 0
        else
          (* Recursively check adjacent positions *)
          let sum_of_neighbors = List.fold_left (fun acc (nx, ny) -> acc + sum_of_part_numbers schematic nx ny) 0 neighbors in

          sum_of_neighbors
    end
  else
    0

(* Function to calculate the sum of all part numbers in the engine schematic *)
let engine_schematic_sum schematic =
  let rows = Array.length schematic in
  let cols = Array.length schematic.(0) in
  let total_sum = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      total_sum := !total_sum + sum_of_part_numbers schematic i j
    done
  done;

  !total_sum

let input_to_schematic input =
  let ic = open_in input in
  try
    let lines = In_channel.input_lines ic in
    close_in ic;
    lst |> List.map String.to_seq |> List.map Array.of_seq |> Array.of_list |> list_of_strings_to_char_array_array
  with e ->
    close_in_noerr ic;
    raise e


let () =
  let result = "day03a-input.txt" |> input_to_schematic |> engine_schematic_sum in
  Printf.printf "%d\n" result
