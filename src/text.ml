(* A buffer is a sequence of lines, and lines are a sequence of characters. *)

type character = string

type line = character Sequence.t

type t = line Sequence.t

let empty = Sequence.empty

(* TODO: concurrent loading. *)
let load_channel ch =
  (* Input whole channel as a string. *)
  let buffer = Buffer.create 4096 in
  let bytes = Bytes.create 1024 in
  let rec loop () =
    let len = input ch bytes 0 (Bytes.length bytes) in
    if len > 0 then (
      Buffer.add_subbytes buffer bytes 0 len;
      loop ()
    )
  in
  loop ();
  let string = Buffer.contents buffer in

  (* Split characters. *)
  let characters = Utf8.split_runes string in

  (* Split characters into lines. *)
  let line = ref [] in
  let text = ref [] in
  let add_character character =
    if character = "\n" then (
      (* TODO: Sequence.of_list_rev (requires Sequence.of_array_rev?) *)
      text := Sequence.of_list (List.rev !line) :: !text;
      line := []
    ) else
      line := character :: !line
  in
  List.iter add_character characters;

  (* Add last line. *)
  Sequence.of_list (List.rev (Sequence.of_list (List.rev !line) :: !text))

let load_file filename =
  let ch = open_in filename in
  try
    let result = load_channel ch in
    close_in ch;
    result
  with exn ->
    close_in ch;
    raise exn

let save_channel filename ch (text: t) =
  let first = ref true in
  let output_line (line: line) =
    if !first then
      first := false
    else
      output_char ch '\n';
    let output_character (character: character) =
      output_string ch character
    in
    Sequence.iter output_character line
  in
  Sequence.iter output_line text

let save_file filename (text: t) =
  let ch = open_out filename in
  try
    save_channel filename ch text;
    close_out ch
  with exn ->
    close_out ch;
    raise exn

let get (x, y) (text: t) =
  match Sequence.get y text with
    | None ->
        " "
    | Some line ->
        match Sequence.get x line with
          | None ->
              " "
          | Some character ->
              character

let line_count (text: t) =
  Sequence.count text

let line_length y (text: t) =
  match Sequence.get y text with
    | None ->
        0
    | Some line ->
        Sequence.count line

let insert (x, y) character (text: t) =
  match Sequence.get y text with
    | None ->
        (* No line at [y], insert new line. *)
        Sequence.insert y (Sequence.one character) text
    | Some line ->
        (* Insert in line. *)
        let line = Sequence.insert x character line in
        (* Replace line. *)
        Sequence.set y line text

let split_line (x, y) (text: t) =
  let text = Sequence.insert (y + 1) Sequence.empty text in
  let line_to_split =
    match Sequence.get y text with
      | None ->
          Sequence.empty
      | Some line ->
          line
  in
  let left, right = Sequence.split x line_to_split in
  let text = Sequence.set y left text in
  Sequence.set (y + 1) right text

let remove (x, y) (text: t) =
  match Sequence.get y text with
    | None ->
        (* No line at [y], do nothing. *)
        text
    | Some line ->
        (* Remove from line. *)
        let line = Sequence.remove x line in
        (* Replace line. *)
        Sequence.set y line text

let remove_line y (text: t) =
  Sequence.remove y text

let merge_lines y (text: t) =
  match Sequence.get y text, Sequence.get (y + 1) text with
    | Some line1, Some line2 ->
        let text = Sequence.remove (y + 1) text in
        Sequence.set y (Sequence.concat line1 line2) text
    | _ ->
        text

let show_line (line: line) =
  Sequence.show (Printf.sprintf "%S") line

let show (text: t) =
  Sequence.show show_line text

let pretty_line fmt (line: line) =
  Sequence.pretty (fun fmt -> Format.fprintf fmt "%S") fmt line

let pretty fmt (text: t) =
  Sequence.pretty pretty_line fmt text

let get_line y (text: t) =
  match Sequence.get y text with
    | None ->
        Sequence.empty
    | Some line ->
        line

let sub (x1, y1) (x2, y2) (text: t) =
  if y1 > y2 then
    empty
  else if y1 = y2 then
    Sequence.one (Sequence.sub x1 x2 (get_line y1 text))
  else
    let first =
      let line = get_line y1 text in
      Sequence.sub x1 (Sequence.count line - 1) line
    in
    let middle =
      Sequence.sub (y1 + 1) (y2 - 1) text
    in
    let last =
      let line = get_line y2 text in
      Sequence.sub 0 x2 line
    in
    middle
    |> Sequence.prepend first
    |> Sequence.append last

let insert_sub (x, y) (sub: t) (text: t): t =
  let line = get_line y text in
  let left, right = Sequence.split x line in

  (* Prepend [left] to the first line of [sub]. *)
  let sub =
    match Sequence.get 0 sub with
      | None ->
          Sequence.one left
      | Some first_line ->
          Sequence.set 0 (Sequence.concat left first_line) sub
  in

  (* Append [right] to the last line of [sub]. *)
  let sub =
    let last_index = Sequence.count sub - 1 in
    match Sequence.get last_index sub with
      | None ->
          assert false (* Impossible, we added a line above. *)
      | Some last_line ->
          Sequence.set last_index (Sequence.concat last_line right) sub
  in

  (* Remove line at [y]. *)
  let text = Sequence.remove y text in

  (* Replace it by inserting the lines of [sub]. *)
  Sequence.insert_sub y sub text
