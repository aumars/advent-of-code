let check_xmas_sequence mat i j di dj =
  let m, n = Array.length mat, Array.length mat.(0) in
  let in_bounds =
    0 <= i + 3*di && i + 3*di < m &&
    0 <= j + 3*dj && j + 3*dj < n
  in
  in_bounds && (
    (mat.(i).(j) = 'X' &&
     mat.(i+di).(j+dj) = 'M' &&
     mat.(i+2*di).(j+2*dj) = 'A' &&
     mat.(i+3*di).(j+3*dj) = 'S') ||
    (mat.(i).(j) = 'S' &&
     mat.(i+di).(j+dj) = 'A' &&
     mat.(i+2*di).(j+2*dj) = 'M' &&
     mat.(i+3*di).(j+3*dj) = 'X')
  )

let count_xmas mat check_sequence =
  let m, n = Array.length mat, Array.length mat.(0) in
  let directions = [
    (0, 1);    (* horizontal *)
    (1, 0);    (* vertical *)
    (1, 1);    (* diagonal down-right *)
    (1, -1)    (* diagonal down-left *)
  ] in
  let count = ref 0 in

  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      List.iter (fun (di, dj) ->
        if check_sequence mat i j di dj then
          incr count
      ) directions
    done
  done;

  !count

let parse_in input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun line -> line <> String.empty)
  |> List.map (fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_list

let () =
  if Array.length Sys.argv != 2 then
    raise (Invalid_argument "Puzzle input file required.")
  else
    let grid =
      Sys.argv.(1)
      |> fun file -> In_channel.with_open_text file In_channel.input_all
      |> parse_in
    in
    Printf.printf "Part One: %d\n" @@ count_xmas grid check_xmas_sequence;
