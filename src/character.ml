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

module Set =
struct
  include Set.Make (M)

  let ascii_range (a: char) (b: char): t =
    let a = Char.code a in
    let b = Char.code b in
    let rec loop acc b =
      if b >= a then
        loop (add (String.make 1 (Char.chr b)) acc) (b - 1)
      else
        acc
    in
    loop empty b
end

module Map = Map.Make (M)
