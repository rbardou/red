type t = Character.t Sequence.t

let empty = Sequence.empty

let one = Sequence.one

let of_list text = Sequence.of_list text

let of_utf8_string string =
  of_list (Utf8.split_runes string)

let get = Sequence.get

let length = Sequence.count

let insert = Sequence.insert

let split = Sequence.split

let split_left = Sequence.split_left

let split_right = Sequence.split_right

let concat = Sequence.concat

let output_channel ch line =
  let output_character character = output_string ch character in
  Sequence.iter output_character line

let sub = Sequence.sub
