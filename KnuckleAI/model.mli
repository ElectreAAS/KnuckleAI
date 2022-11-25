type column = { contents : int list; score : int }
type choice = Left | Middle | Right
type player = Ratau | Lamb
type board = { left : column; middle : column; right : column }
type t = { ratau : board; lamb : board; up_next : player; next_dice : int }
type ai = t -> choice

val init : ?up_next:string -> unit -> t
val choice_of_string : string -> choice
val is_valid : t -> choice -> bool
val is_finished : t -> bool
val play : t -> choice -> t * bool
val scores : t -> int * int
val display : t -> unit