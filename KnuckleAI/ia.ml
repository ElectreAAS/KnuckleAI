let choose list = List.nth list (Random.int (List.length list))
let options = Model.[ Left; Middle; Right ]
let score (l, r) = function Model.Ratau -> l - r | Lamb -> r - l

(* IA nulle *)
let random t = options |> List.filter (Model.is_valid t) |> choose

(* IA moins nulle *)
let rec random_game joueur plateau =
  let options = List.filter (Model.is_valid plateau) options in
  let next, finished = Model.play plateau (choose options) in
  if finished then score (Model.scores next) joueur else random_game joueur next

let value j t choice =
  let next, finished = Model.play t choice in
  if finished then score (Model.scores next) j |> Float.of_int
  else
    let n_plays = 100000 in
    let total = ref 0 in
    for _ = 1 to n_plays do
      total := !total + random_game j next
    done;
    Float.of_int !total /. Float.of_int n_plays

let to_string = function Model.Left -> "⬅️ " | Middle -> "⬆️ " | Right -> "➡️ "

let alpha_go ({ Model.up_next; _ } as t) =
  let res =
    options
    |> List.filter (Model.is_valid t)
    |> List.fold_left
         (fun (cur_max, cur_action) c ->
           let v = value up_next t c in
           Printf.printf "- %s| %f\n" (to_string c) v;
           if v > cur_max then (v, Some c) else (cur_max, cur_action))
         (-100., None)
    |> snd
    |> Option.get
  in
  Printf.printf ":: %s\n" (to_string res);
  res
