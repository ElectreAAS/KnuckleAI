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
    |> snd |> Option.get
  in
  Printf.printf ":: %s\n" (to_string res);
  res

(* monte carle tree search *)
let hash_column { Model.contents; _ } num =
  List.fold_left (fun num c -> (num lsl 3) lor c) num contents

let hash_board { Model.left; middle; right } num =
  num |> hash_column left |> hash_column middle |> hash_column right

let to_int = function Model.Left -> 0 | Middle -> 1 | Right -> 2

let hash (t : Model.t) c =
  let h =
    if t.up_next = Ratau then hash_board t.ratau 0 |> hash_board t.lamb
    else hash_board t.lamb 0 |> hash_board t.ratau
  in
  (((h lsl 3) lor t.next_dice) lsl 3) lor to_int c

type evaluation =
  | Node of { mean_score : float; n_iter : int }
  | Finished of { score : int }

let ev_to_string = function
  | Node { n_iter; _ } -> "n_iter:" ^ string_of_int n_iter
  | Finished _ -> "finished"

let state : (int, evaluation) Hashtbl.t = Hashtbl.create 10000

let uct ?explore t =
  match (t, explore) with
  | Finished { score }, _ -> Float.of_int score
  | Node { mean_score; n_iter = _ }, None -> mean_score
  | Node { mean_score; n_iter }, Some n_iter_parent ->
      if n_iter == 0 then 1000.
      else
        mean_score
        +. 50.
           *. Float.sqrt
                (Float.log (Float.of_int (1 + n_iter_parent))
                /. Float.of_int n_iter)

let next_move ?explore (t : Model.t) =
  assert (not (Model.is_finished t));
  let player = t.up_next in
  let options = List.filter (Model.is_valid t) options in
  let total_n_iter = ref 0 in
  let states_evaluation =
    List.map
      (fun c ->
        let hash = hash t c in
        match Hashtbl.find_opt state hash with
        | Some (Node { n_iter; _ } as n) ->
            total_n_iter := !total_n_iter + n_iter;
            (c, n)
        | Some v -> (c, v)
        | None ->
            let res, finished = Model.play t c in
            let r =
              if finished then
                let score = score (Model.scores res) player in
                Finished { score }
              else Node { mean_score = 0.; n_iter = 0 }
            in
            (c, r))
      options
  in
  let explore = Option.map (fun () -> !total_n_iter) explore in
  let c, t =
    states_evaluation
    |> List.fold_left
         (fun (maxc, maxuct) (c, ev) ->
           let uct = uct ?explore ev in
           if Option.is_none explore then
             Printf.printf "[] %s | %f (%s)\n" (to_string c) uct
               (ev_to_string ev);
           if uct > maxuct then (Some c, uct) else (maxc, maxuct))
         (None, -1000.)
  in
  match c with
  | Some c -> c
  | None ->
      failwith
        ("uh "
        ^ Int.to_string (List.length states_evaluation)
        ^ " -- " ^ Float.to_string t)

let rec play (t : Model.t) =
  if Model.is_finished t then score (Model.scores t) t.up_next
  else
    let c = next_move ~explore:() t in
    let t_hash = hash t c in
    match Hashtbl.find_opt state t_hash with
    | None ->
        let score = random_game t.up_next t in
        Hashtbl.add state t_hash
          (Node { mean_score = Float.of_int score; n_iter = 1 });
        score
    | Some (Finished _) -> assert false
    | Some (Node { mean_score; n_iter }) ->
        let next_state, _ = Model.play t c in
        let score = -play next_state in
        let node =
          let mean_score =
            ((mean_score *. Float.of_int n_iter) +. Float.of_int score)
            /. Float.of_int (n_iter + 1)
          in
          Node { mean_score; n_iter = n_iter + 1 }
        in
        Hashtbl.replace state t_hash node;
        score

let monte_carlo t =
  for _ = 0 to 100000 do
    play t |> ignore
  done;
  next_move t
