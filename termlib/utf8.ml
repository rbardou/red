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
        let rec loop rune_acc pos n =
          if n <= 0 then
            rune_acc
          else
            loop (str.[pos] :: rune_acc) (pos + 1) (n - 1)
        in
        let rune_acc = loop [] pos size in
        let rune = Misc.string_of_char_list_rev rune_acc in
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
            if pos + 2 >= len then
              invalid 3
            else
              split_at
                (Misc.string_of_char_list_rev
                   [ str.[pos + 2]; str.[pos + 1]; char ]
                 :: acc)
                (pos + 3)
        | '\240' .. '\247' ->
            if pos + 3 >= len then
              invalid 4
            else
              split_at
                (Misc.string_of_char_list_rev [ str.[pos + 1]; char ] :: acc)
                (pos + 4)
        | '\248' .. '\255' ->
            invalid 1
  in

  List.rev (split_at [] 0)
