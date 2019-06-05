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

let is_big_word_character character =
  String.length character <> 1 ||
  match character.[0] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
    | _ -> false

let is_not_big_word_character character =
  not (is_big_word_character character)

let equals = String.equal

let case_insensitive_equals a b = String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

let compare = String.compare

module M =
struct
  type t = string
  let compare = compare
end

module Map = Map.Make (M)
