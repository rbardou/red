type t = string

let of_ascii = String.make 1

(* For now, we assume all non-ASCII characters to be word characters.
   TODO: use some kind of official list for Unicode? *)
let is_word_character character =
  String.length character <> 1 ||
  match character.[0] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | _ -> false

let is_not_word_character character =
  not (is_word_character character)
