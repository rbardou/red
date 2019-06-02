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

let set x y character text =
  match Sequence.get y text with
    | None ->
        invalid_arg (Printf.sprintf "Text.set %d %d: no such line" x y)
    | Some line ->
        let line = Line.set x character line in
        Sequence.set y line text

let get_line_count text =
  Sequence.count text

let get_line_length y text =
  match Sequence.get y text with
    | None ->
        0
    | Some line ->
        Line.length line

let is_line_empty y text =
  match Sequence.get y text with
    | None ->
        true
    | Some line ->
        Line.is_empty line

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

let sub_region ~x ~y ~characters ~lines text =
  let x2 =
    if lines = 0 then
      x + characters - 1
    else
      characters - 1
  in
  sub ~x1: x ~y1: y ~x2 ~y2: (y + lines) text

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

let append_new_line text =
  Sequence.append Line.empty text

let append_character character text =
  if character = "\n" then
    append_new_line text
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

let map f text =
  Sequence.map (Line.map f) text

let map_sub ~x1 ~y1 ~x2 ~y2 f text =
  if y2 < y1 then
    text
  else if y1 = y2 then
    if x2 < x1 then
      text
    else
      let line = get_line y1 text in
      let line = Line.map_sub x1 x2 f line in
      Sequence.set y1 line text
  else
    (* Map the first line. *)
    let first_line = get_line y1 text in
    let first_line = Line.map_from x1 f first_line in
    let text = Sequence.set y1 first_line text in

    (* Map the middle. *)
    let map_line line = Line.map f line in
    let text = Sequence.map_sub (y1 + 1) (y2 - 1) map_line text in

    (* Map the last line. *)
    let last_line = get_line y2 text in
    let last_line = Line.map_until x2 f last_line in
    Sequence.set y2 last_line text

let concat a b =
  let a_line_count = get_line_count a in
  let a_last_line = get_line (a_line_count - 1) a in
  let b_first_line = get_line 0 b in
  let a = Sequence.set (a_line_count - 1) (Line.concat a_last_line b_first_line) a in
  Sequence.concat a (Sequence.remove 0 b)

let equals equal_characters a b =
  let line_count = get_line_count a in
  if get_line_count b <> line_count then
    false
  else
    let rec loop from =
      if from >= line_count then
        true
      else
        let la = get_line from a in
        let lb = get_line from b in
        if Line.equals equal_characters la lb then
          loop (from + 1)
        else
          false
    in
    loop 0

let prepare_search ?x2 ?y2 ~subtext text =
  let y2 =
    match y2 with
      | None ->
          get_line_count text - 1
      | Some y2 ->
          y2
  in
  let x2 =
    match x2 with
      | None ->
          get_line_length y2 text
      | Some x2 ->
          x2
  in
  let lines = get_line_count subtext - 1 in
  let characters = get_line_length lines subtext in
  x2, y2, lines, characters

let search_found ~x ~y ~lines ~characters =
  let end_x, end_y =
    if lines = 0 then
      x + characters - 1, y
    else
      characters - 1, y + lines
  in
  Some (x, y, end_x, end_y)

(* TODO: Boyer-Moore algorithm or something like that. *)
let search_forwards equal_characters ?(x1 = 0) ?(y1 = 0) ?x2 ?y2 ~subtext text =
  let x2, y2, lines, characters = prepare_search ?x2 ?y2 ~subtext text in
  let rec search x y =
    if y2 < y || y2 = y && x2 < x then
      None
    else
      let candidate = sub_region ~x ~y ~characters ~lines text in
      if equals equal_characters subtext candidate then
        search_found ~x ~y ~lines ~characters
      else
        let x, y =
          if x > get_line_length y text then
            0, y + 1
          else
            x + 1, y
        in
        search x y
  in
  search x1 y1

(* TODO: Boyer-Moore algorithm or something like that. *)
let search_backwards equal_characters ?(x1 = 0) ?(y1 = 0) ?x2 ?y2 ~subtext text =
  let x2, y2, lines, characters = prepare_search ?x2 ?y2 ~subtext text in
  let rec search x y =
    if y < y1 || y = y1 && x < x1 then
      None
    else
      let candidate = sub_region ~x ~y ~characters ~lines text in
      if equals equal_characters subtext candidate then
        search_found ~x ~y ~lines ~characters
      else
        let x, y =
          if x <= 0 then
            get_line_length (y - 1) text, y - 1
          else
            x - 1, y
        in
        search x y
  in
  search x2 y2
