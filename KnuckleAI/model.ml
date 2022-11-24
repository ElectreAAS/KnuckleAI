type column = { contents : int list; score : int }
type board = { left : column; middle : column; right : column }
type player = Ratau | Lamb
type t = { ratau : board; lamb : board; up_next : player }
type choice = Left of int | Middle of int | Right of int

let player_of_string = function
  | "ratau" | "Ratau" -> Ratau
  | "lamb" | "Lamb" -> Lamb
  | _ ->
      Random.self_init ();
      if Random.bool () then Ratau else Lamb

let init ?(up_next = "") () =
  let col = { contents = []; score = 0 } in
  let b = { left = col; middle = col; right = col } in
  { ratau = b; lamb = b; up_next = player_of_string up_next }

let pos_of_choice b = function
  | Left _ -> b.left
  | Middle _ -> b.middle
  | Right _ -> b.right

let pos_of_player t = function Ratau -> t.ratau | Lamb -> t.lamb

let choice_of_string s =
  match String.split_on_char ' ' s with
  | ("left" | "Left") :: [ x ] ->
      let n = int_of_string x in
      Left n
  | ("middle" | "Middle") :: [ x ] ->
      let n = int_of_string x in
      Middle n
  | ("right" | "Right") :: [ x ] ->
      let n = int_of_string x in
      Right n
  | _ -> invalid_arg "Can't parse choice!"

let arr_of_board b =
  let arr = Array.make 3 (Array.make 4 0) in
  let push col i =
    for j = 0 to 2 do
      List.nth_opt col.contents j |> Option.iter (Array.set arr.(i) j)
    done;
    arr.(i).(3) <- col.score
  in
  push b.left 0;
  push b.middle 1;
  push b.right 2;
  arr

let score_of_col = function
  | [ x; y ] when x = y -> x * 4
  | [ x; y; z ] when x = y && x = z -> x * 9
  | [ x; y; z ] when x = y -> (x * 4) + z
  | [ x; y; z ] when x = z -> (x * 4) + y
  | [ x; y; z ] when y = z -> (y * 4) + x
  | col -> List.fold_left ( + ) 0 col

let play_self b = function
  | Left n ->
      let contents = n :: b.left.contents in
      let score = score_of_col contents in
      { b with left = { contents; score } }
  | Middle n ->
      let contents = n :: b.middle.contents in
      let score = score_of_col contents in
      { b with middle = { contents; score } }
  | Right n ->
      let contents = n :: b.right.contents in
      let score = score_of_col contents in
      { b with right = { contents; score } }

let oppo_played n col =
  let contents = List.filter (( <> ) n) col.contents in
  let score = score_of_col contents in
  { contents; score }

let play_oppo b = function
  | Left n ->
      let left = oppo_played n b.left in
      { b with left }
  | Middle n ->
      let middle = oppo_played n b.middle in
      { b with middle }
  | Right n ->
      let right = oppo_played n b.right in
      { b with right }

let is_valid (t : t) ch =
  let board = pos_of_player t t.up_next in
  let col = pos_of_choice board ch in
  List.length col.contents < 3

let is_finished t =
  let is_finished_board b =
    List.length b.left.contents
    + List.length b.middle.contents
    + List.length b.right.contents
    = 9
  in
  is_finished_board t.ratau || is_finished_board t.lamb

let play t ch =
  assert (is_valid t ch);
  assert (not @@ is_finished t);
  let ratau, lamb, up_next =
    match t.up_next with
    | Ratau -> (play_self t.ratau ch, play_oppo t.lamb ch, Lamb)
    | Lamb -> (play_oppo t.ratau ch, play_self t.lamb ch, Ratau)
  in
  { ratau; lamb; up_next }

let scores t =
  let score_of_board b = b.left.score + b.middle.score + b.left.score in
  (score_of_board t.ratau, score_of_board t.lamb)

let display t =
  let rarr, larr = (arr_of_board t.ratau, arr_of_board t.lamb) in
  let rscore, lscore = scores t in
  let remoji, lemoji =
    match compare rscore lscore with
    | 0 -> (":|", ":|")
    | 1 -> (":)", ":(")
    | _ -> (":(", ":)")
  in
  let top_cell = function 0 -> "       " | _ -> " ╭───╮ " in
  let mid_cell = function 0 -> "       " | n -> Printf.sprintf " │ %d │ " n in
  let bot_cell = function 0 -> "       " | _ -> " ╰───╯ " in
  let print_rows arr =
    for n = 0 to 2 do
      Printf.printf "║%s┊%s┊%s║\n"
        (top_cell arr.(0).(n))
        (top_cell arr.(1).(n))
        (top_cell arr.(2).(n));
      Printf.printf "║%s┊%s┊%s║\n"
        (mid_cell arr.(0).(n))
        (mid_cell arr.(1).(n))
        (mid_cell arr.(2).(n));
      Printf.printf "║%s┊%s┊%s║\n"
        (bot_cell arr.(0).(n))
        (bot_cell arr.(1).(n))
        (bot_cell arr.(2).(n))
    done
  in
  let print_grid = function
    | Ratau ->
        print_string "\n╔═══════════════════════╗\n";
        print_rows rarr;
        Printf.printf "╚═ %2d ════ %2d ════ %2d ══╝\n"
          rarr.(0).(3)
          rarr.(1).(3)
          rarr.(2).(3);
        Printf.printf "\n          %3d %s\n" rscore remoji
    | Lamb ->
        Printf.printf "\n          %3d %s\n" lscore lemoji;
        Printf.printf "╔═ %2d ════ %2d ════ %2d ══╗\n"
          larr.(0).(3)
          larr.(1).(3)
          larr.(2).(3);
        print_rows larr;
        print_string "╚═══════════════════════╝\n"
  in
  print_grid Ratau;
  print_string "\n 🎲🎲🎲🎲🎲🎲🎲🎲🎲🎲🎲🎲\n\n";
  print_grid Lamb;
  flush stdout