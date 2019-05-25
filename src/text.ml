type 'a t = 'a Line.t Sequence.t

let empty = Sequence.one Line.empty

let one_line = Sequence.one

let output_channel ch text =
  let first = ref true in
  let output_line line =
    if !first then
      first := false
    else
      output_char ch '\n';
    Line.output_channel ch line
  in
  Sequence.iter output_line text

let output_file ?perm filename text =
  System.with_open_out ?perm filename @@ fun ch ->
  output_channel ch text

let to_string text =
  (* TODO: compute buffer size? We expect [to_string] to be used for small texts only. *)
  let buf = Buffer.create 512 in
  let first = ref true in
  let add_line line =
    if !first then
      first := false
    else
      Buffer.add_char buf '\n';
    Line.to_buffer buf line
  in
  Sequence.iter add_line text;
  Buffer.contents buf

let of_utf8_string string =
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

let of_utf8_substrings_offset_0 substrings =
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
  of_utf8_string string

let get x y text =
  match Sequence.get y text with
    | None ->
        None
    | Some line ->
        Line.get x line

let get_line_count text =
  Sequence.count text

let get_line_length y text =
  match Sequence.get y text with
    | None ->
        0
    | Some line ->
        Line.length line

let insert x y character text =
  match Sequence.get y text with
    | None ->
        text
    | Some line ->
        (* Insert in line. *)
        let line = Line.insert x character line in
        (* Replace line. *)
        Sequence.set y line text

let insert_new_line x y text =
  match Sequence.get y text with
    | None ->
        text
    | Some line ->
        (* Split line, only keep the left part in it. *)
        let left, right = Line.split x line in
        let text = Sequence.set y left text in
        (* Add the right part as a new line. *)
        Sequence.insert (y + 1) right text

let delete_region ~x ~y ~characters ~lines text =
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

let get_line y text =
  match Sequence.get y text with
    | None ->
        Line.empty
    | Some line ->
        line

let sub ~x1 ~y1 ~x2 ~y2 text =
  if y1 > y2 then
    empty
  else if y1 = y2 then
    Sequence.one (Line.sub x1 x2 (get_line y1 text))
  else
    let first =
      let line = get_line y1 text in
      Line.sub x1 (Line.length line - 1) line
    in
    let middle =
      Sequence.sub (y1 + 1) (y2 - 1) text
    in
    let last =
      let line = get_line y2 text in
      Line.sub 0 x2 line
    in
    middle
    |> Sequence.prepend first
    |> Sequence.append last

let insert_text ~x ~y ~sub text =
  let line = get_line y text in
  let left, right = Line.split x line in

  (* Prepend [left] to the first line of [sub]. *)
  let sub =
    match Sequence.get 0 sub with
      | None ->
          Sequence.one left
      | Some first_line ->
          Sequence.set 0 (Line.concat left first_line) sub
  in

  (* Append [right] to the last line of [sub]. *)
  let sub =
    let last_index = Sequence.count sub - 1 in
    match Sequence.get last_index sub with
      | None ->
          assert false (* Impossible, we added a line above. *)
      | Some last_line ->
          Sequence.set last_index (Line.concat last_line right) sub
  in

  (* Remove line at [y]. *)
  let text = Sequence.remove y text in

  (* Replace it by inserting the lines of [sub]. *)
  Sequence.insert_sub y sub text

let append_character character text =
  if character = "\n" then
    Sequence.append Line.empty text
  else
    let last_line_index = Sequence.count text - 1 in
    let last_line =
      match Sequence.get last_line_index text with
        | None ->
            Line.empty
        | Some line ->
            line
    in
    Sequence.set last_line_index (Line.append character last_line) text
