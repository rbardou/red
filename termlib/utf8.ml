let split_runes ?(invalid_rune = Some "�") str =
  let len = String.length str in

  let rec split_at acc pos =
    (* When detecting an invalid rune, skip it, possibly replace it. *)
    let invalid rune_length =
      let acc =
        match invalid_rune with
          | None ->
              acc
          | Some invalid_rune ->
              invalid_rune :: acc
      in
      split_at acc (pos + rune_length)
    in

    (* Read a rune with a length of more than one character. *)
    let rune size =
      if pos + size > len then
        invalid size
      else
        let rune = String.sub str pos size in
        split_at (rune :: acc) (pos + size)
    in

    if pos >= len then
      acc
    else
      let char = str.[pos] in
      match char with
        | '\000' .. '\127' ->
            split_at (String.make 1 char :: acc) (pos + 1)
        | '\128' .. '\191' ->
            invalid 1
        | '\192' .. '\223' ->
            rune 2
        | '\224' .. '\240' ->
            rune 3
        | '\240' .. '\247' ->
            rune 4
        | '\248' .. '\255' ->
            invalid 1
  in

  List.rev (split_at [] 0)

(* TODO: We can probably be much more efficient. *)
type parser_state =
  | Started
  | Missing_1_of_2 of char
  | Missing_2_of_3 of char
  | Missing_1_of_3 of char * char
  | Missing_3_of_4 of char
  | Missing_2_of_4 of char * char
  | Missing_1_of_4 of char * char * char
  | Completed_ASCII of char
  | Completed_Unicode of string
  | Invalid

let add_char char state =
  match state with
    | Started | Completed_ASCII _ | Completed_Unicode _ | Invalid ->
        (
          match char with
            | '\000' .. '\127' ->
                Completed_ASCII char
            | '\128' .. '\191' ->
                Invalid
            | '\192' .. '\223' ->
                Missing_1_of_2 char
            | '\224' .. '\240' ->
                Missing_2_of_3 char
            | '\240' .. '\247' ->
                Missing_3_of_4 char
            | '\248' .. '\255' ->
                Invalid
        )
    | Missing_1_of_2 first ->
        let bytes = Bytes.create 2 in
        Bytes.set bytes 0 first;
        Bytes.set bytes 1 char;
        Completed_Unicode (Bytes.unsafe_to_string bytes)
    | Missing_2_of_3 first ->
        Missing_1_of_3 (first, char)
    | Missing_1_of_3 (first, second) ->
        let bytes = Bytes.create 3 in
        Bytes.set bytes 0 first;
        Bytes.set bytes 1 second;
        Bytes.set bytes 2 char;
        Completed_Unicode (Bytes.unsafe_to_string bytes)
    | Missing_3_of_4 first ->
        Missing_2_of_4 (first, char)
    | Missing_2_of_4 (first, second) ->
        Missing_1_of_4 (first, second, char)
    | Missing_1_of_4 (first, second, third) ->
        let bytes = Bytes.create 4 in
        Bytes.set bytes 0 first;
        Bytes.set bytes 1 second;
        Bytes.set bytes 2 third;
        Bytes.set bytes 3 char;
        Completed_Unicode (Bytes.unsafe_to_string bytes)
