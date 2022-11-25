type t
type choice

val init : ?up_next:string -> unit -> t
val choice_of_string : string -> choice
val is_valid : t -> choice -> bool
val is_finished : t -> bool
val play : t -> choice -> t * bool
val scores : t -> int * int
val display : t -> unit