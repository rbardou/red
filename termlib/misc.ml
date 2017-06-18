let string_of_char_list_rev l =
  let len = List.length l in
  let bytes = Bytes.create len in
  let rec loop index l =
    if index >= 0 then
      match l with
        | [] ->
            assert false
        | head :: tail ->
            Bytes.set bytes index head;
            loop (index - 1) tail
  in
  loop (len - 1) l;
  Bytes.unsafe_to_string bytes
