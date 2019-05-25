include Sequence

let length = count

let of_utf8_string string =
  of_list (Utf8.split_runes string)

let to_buffer buf line =
  Sequence.iter (Buffer.add_string buf) line

let output_channel ch line =
  let output_character character = output_string ch character in
  Sequence.iter output_character line
