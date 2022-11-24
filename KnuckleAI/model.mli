type t
type choice
type player

val init : ?up_next:string -> unit -> t
val choice_of_string : string -> choice
val play : t -> choice -> t
val display : t -> unit