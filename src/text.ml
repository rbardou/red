type t = Line.t Sequence.t

let empty = Sequence.one Line.empty

let output_channel ch (text: t) =
  let first = ref true in
  let output_line line =
    if !first then
      first := false
    else
      output_char ch '\n';
    Line.output_channel ch line
  in
  Sequence.iter output_line text

let output_file filename (text: t) =
  let ch = open_out filename in
  try
    output_channel ch text;
    close_out ch;
    Ok ()
  with exn ->
    close_out ch;
    Error exn

let of_utf8_substrings_offset_0 substrings =
  (* Make a single string so that we can call [Utf8.split_runes]. *)
  let string =
    let total_size = List.fold_left (fun acc (_, len) -> acc + len) 0 substrings in
    let bytes = Bytes.create total_size in
    let rec loop offset substrings =
      match substrings with
        | [] ->
            ()
        | (string, len) :: tail ->
            Bytes.blit_string string 0 bytes offset len;
            loop (offset + len) tail
    in
    loop 0 substrings;
    Bytes.unsafe_to_string bytes
  in

  (* Split characters. *)
  let characters = Utf8.split_runes string in

  (* Split characters into lines. *)
  let line = ref [] in
  let text = ref [] in
  let add_character character =
    if character = "\n" then (
      (* TODO: Sequence.of_list_rev (requires Sequence.of_array_rev?) *)
      text := Line.of_list (List.rev !line) :: !text;
      line := []
    ) else
      line := character :: !line
  in
  List.iter add_character characters;

  (* Add last line. *)
  Sequence.of_list (List.rev (Line.of_list (List.rev !line) :: !text))

let get x y (text: t) =
  match Sequence.get y text with
    | None ->
        None
    | Some line ->
        Line.get x line

let get_line_count (text: t) =
  Sequence.count text

let get_line_length y (text: t) =
  match Sequence.get y text with
    | None ->
        0
    | Some line ->
        Line.length line

let insert_character x y character (text: t) =
  match Sequence.get y text with
    | None ->
        text
    | Some line ->
        (* Insert in line. *)
        let line = Line.insert x character line in
        (* Replace line. *)
        Sequence.set y line text

let insert_new_line x y (text: t) =
  match Sequence.get y text with
    | None ->
        text
    | Some line ->
        (* Split line, only keep the left part in it. *)
        let left, right = Line.split x line in
        let text = Sequence.set y left text in
        (* Add the right part as a new line. *)
        Sequence.insert (y + 1) right text

let delete_region ~x ~y ~characters ~lines (text: t) =
  if lines = 0 then
    (* Delete in the middle of line [y]. *)
      match Sequence.get y text with
        | None ->
            text
        | Some line ->
            let left, middle_and_right = Line.split x line in
            let right = Line.split_right characters middle_and_right in
            let line = Line.concat left right in
            Sequence.set y line text

  else
    (* Partially delete first line. *)
    let first_line =
      match Sequence.get y text with
        | None ->
            Line.empty
        | Some line ->
            Line.split_left x line
    in

    let rec remove_lines count text =
      if count < 1 then
        (* Partially delete last line and append it to the first line. *)
        match Sequence.get (y + 1) text with
          | None ->
              text
          | Some line ->
              let text = Sequence.remove (y + 1) text in
              let line = Line.split_right characters line in
              Sequence.set y (Line.concat first_line line) text

      else
        (* Delete one full line and continue. *)
        let text = Sequence.remove (y + count) text in
        remove_lines (count - 1) text
    in
    remove_lines (lines - 1) text
