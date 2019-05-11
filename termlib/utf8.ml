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
      if pos + size >= len then
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
