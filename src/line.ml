type t = Character.t Sequence.t

let empty = Sequence.empty

let one = Sequence.one

let of_list list = Sequence.of_list list

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
