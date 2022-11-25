type t
type choice

val init : ?up_next:string -> unit -> t
val choice_of_string : string -> choice
val is_valid : t -> choice -> bool
val play : t -> choice -> t
val display : t -> unit