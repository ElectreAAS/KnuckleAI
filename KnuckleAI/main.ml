open Model

let rec read_choice () =
  let str = read_line () in
  try choice_of_string str
  with Failure _ ->
    Printf.printf
      "Please enter a column (left, middle, or right) followed by an integer \
       (1-6)\n\
       %!";
    read_choice ()

let rec game_loop t print =
  if print then display t;
  Printf.printf "Please enter your next move\n%!";
  let choice = read_choice () in
  if not @@ is_valid t choice then (
    Printf.printf "This move is invalid, try again\n";
    game_loop t false)
  else game_loop (play t choice) true

let () =
  let t = init () in
  try game_loop t true with Exit -> Printf.printf "Game finished! Well done!"
