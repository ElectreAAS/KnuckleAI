open Model

let go _ = choice_of_string [| "l"; "m"; "r" |].(Random.int 3)