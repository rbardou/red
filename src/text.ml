type t = Line.t Sequence.t

let empty = Sequence.one Line.empty

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
