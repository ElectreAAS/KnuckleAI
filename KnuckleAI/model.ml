type column = { contents : int list; score : int }
type choice = Left | Middle | Right
type player = Ratau | Lamb

type board = { left : column; middle : column; right : column }
and ai = t -> int -> choice
and t = { ratau : board; lamb : board; up_next : player; next_dice : int }

let player_of_string = function
  | "ratau" | "Ratau" -> Ratau
  | "lamb" | "Lamb" -> Lamb
  | _ -> if Random.bool () then Ratau else Lamb

let dice_throw () = Random.int 6 + 1

let init ?(up_next = "") () =
  let col = { contents = []; score = 0 } in
  let b = { left = col; middle = col; right = col } in
  {
    ratau = b;
    lamb = b;
    up_next = player_of_string up_next;
    next_dice = dice_throw ();
  }

let pos_of_choice b = function
  | Left -> b.left
  | Middle -> b.middle
  | Right -> b.right

let pos_of_player t = function Ratau -> t.ratau | Lamb -> t.lamb

let choice_of_string s =
  match s with
  | "l" | "left" | "Left" -> Left
  | "m" | "middle" | "Middle" -> Middle
  | "r" | "right" | "Right" -> Right
  | _ -> failwith "choice_of_string"

let arr_of_board b =
  let arr = Array.init 3 (fun _ -> Array.make 4 0) in
  let push col i =
    List.iteri (fun j x -> arr.(i).(j) <- x) (List.rev col.contents);
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

let play_self b n = function
  | Left ->
      let contents = n :: b.left.contents in
      let score = score_of_col contents in
      { b with left = { contents; score } }
  | Middle ->
      let contents = n :: b.middle.contents in
      let score = score_of_col contents in
      { b with middle = { contents; score } }
  | Right ->
      let contents = n :: b.right.contents in
      let score = score_of_col contents in
      { b with right = { contents; score } }

let oppo_played n col =
  let contents = List.filter (( <> ) n) col.contents in
  let score = score_of_col contents in
  { contents; score }

let play_oppo b n = function
  | Left ->
      let left = oppo_played n b.left in
      { b with left }
  | Middle ->
      let middle = oppo_played n b.middle in
      { b with middle }
  | Right ->
      let right = oppo_played n b.right in
      { b with right }

let is_valid t ch =
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
  let ratau, lamb, up_next =
    match t.up_next with
    | Ratau ->
        (play_self t.ratau t.next_dice ch, play_oppo t.lamb t.next_dice ch, Lamb)
    | Lamb ->
        ( play_oppo t.ratau t.next_dice ch,
          play_self t.lamb t.next_dice ch,
          Ratau )
  in
  let new_t = { ratau; lamb; up_next; next_dice = dice_throw () } in
  (new_t, is_finished new_t)

let scores t =
  let score_of_board b = b.left.score + b.middle.score + b.right.score in
  (score_of_board t.ratau, score_of_board t.lamb)

let display t =
  let rarr, larr = (arr_of_board t.ratau, arr_of_board t.lamb) in
  let rscore, lscore = scores t in
  let over = is_finished t in
  let remoji, lemoji =
    match compare rscore lscore with
    | 0 -> if over then ("TIE 🙃", "TIE 🙃") else ("😐", "😐")
    | 1 -> if over then ("WINNER 😎", "LOSER  😞") else ("😃", "😟")
    | _ -> if over then ("LOSER  😠", "WINNER 🥳") else ("😟", "😃")
  in
  let top_cell = function 0 -> "       " | _ -> " ╭───╮ " in
  let mid_cell = function 0 -> "       " | n -> Printf.sprintf " │ %d │ " n in
  let bot_cell = function 0 -> "       " | _ -> " ╰───╯ " in
  let print_rows arr upside_down =
    let contents n =
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
    in
    if upside_down then
      for n = 0 to 2 do
        contents n
      done
    else
      for n = 2 downto 0 do
        contents n
      done
  in
  let print_grid = function
    | Ratau ->
        print_string "\n╔═══════╤═══════╤═══════╗\n";
        print_rows rarr false;
        Printf.printf "╚═ %2d ══╧═ %2d ══╧═ %2d ══╝\n"
          rarr.(0).(3)
          rarr.(1).(3)
          rarr.(2).(3);
        Printf.printf "    Ratau:%3d %s\n" rscore remoji
    | Lamb ->
        Printf.printf "     Lamb:%3d %s\n" lscore lemoji;
        Printf.printf "╔═ %2d ══╤═ %2d ══╤═ %2d ══╗\n"
          larr.(0).(3)
          larr.(1).(3)
          larr.(2).(3);
        print_rows larr true;
        print_string "╚═══════╧═══════╧═══════╝\n"
  in
  print_grid Ratau;
  (if not over then
   let next_emoji = match t.up_next with Ratau -> "⬆️" | Lamb -> "⬇️" in
   Printf.printf "\nYour turn, %s  You rolled %d\n\n" next_emoji t.next_dice);
  print_grid Lamb;
  flush stdout