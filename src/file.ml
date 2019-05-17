type mark =
  {
    mutable x: int;
    mutable y: int;
  }

type cursor =
  {
    selection_start: mark;
    position: mark;
    mutable preferred_x: int;
  }

(* Test whether a mark is before another mark. *)
let (<%) m1 m2 = m1.y < m2.y || (m1.y = m2.y && m1.x < m2.x)
let (<=%) m1 m2 = m1.y < m2.y || (m1.y = m2.y && m1.x <= m2.x)

let selection_boundaries cursor =
  if cursor.position <% cursor.selection_start then
    cursor.position, cursor.selection_start
  else
    cursor.selection_start, cursor.position

let cursor_is_in_selection x y cursor =
  let left, right = selection_boundaries cursor in
  let xy = { x; y } in
  left <=% xy && xy <% right

let selection_is_empty cursor =
  cursor.position.x = cursor.selection_start.x &&
  cursor.position.y = cursor.selection_start.y

type t =
  {
    mutable views: view list;
    mutable text: Text.t;
    mutable modified: bool;
    mutable filename: string option;

    (* If [loading] is [Some (loaded, size, sub_strings_rev)], only [loaded] bytes out of [size]
       have been loaded, and [text] is read-only. *)
    mutable loading: (int * int * (string * int) list) option;
  }

and view =
  {
    file: t;
    mutable scroll_x: int;
    mutable scroll_y: int;
    mutable width: int; (* set when rendering *)
    mutable height: int; (* set when rendering *)
    mutable marks: mark list;
    mutable cursors: cursor list;
  }

let create_cursor x y =
  {
    selection_start = { x; y };
    position = { x; y };
    preferred_x = x;
  }

let create_view file =
  let cursor = create_cursor 0 0 in
  let view =
    {
      file;
      scroll_x = 0;
      scroll_y = 0;
      width = 80;
      height = 40;
      marks = [ cursor.selection_start; cursor.position ];
      cursors = [ cursor ];
    }
  in
  file.views <- view :: file.views;
  view

let create text =
  {
    views = [];
    text;
    modified = false;
    filename = None;
    loading = None;
  }

let create_loading filename =
  let file_descr =
    (* TODO: error handling (e.g. file does not exist) *)
    Unix.openfile filename [ O_RDONLY ] 0o640
  in
  let size =
    (* TODO: error handling *)
    (Unix.fstat file_descr).st_size
  in

  let file = create Text.empty in
  file.filename <- Some filename;
  file.loading <- Some (0, size, []);

  let rec load () =
    (* TODO: group, so that if we kill this file, we kill the reader *)
    Spawn.on_read file_descr @@ fun () ->
    match file.loading with
      | None ->
          assert false (* did someone else temper with this file? *)
      | Some (loaded, size, sub_strings_rev) ->
          let bytes = Bytes.create 8192 in
          let len = Unix.read file_descr bytes 0 8192 in
          if len = 0 then
            (* TODO: [Text.of_utf8_substrings_offset_0] can block for a while.
               Replace it by some kind of iterative parser. *)
            let text = Text.of_utf8_substrings_offset_0 (List.rev sub_strings_rev) in
            file.text <- text;
            file.loading <- None
          else
            let sub_strings_rev = (Bytes.unsafe_to_string bytes, len) :: sub_strings_rev in
            file.loading <- Some (loaded + len, size, sub_strings_rev);
            load ()
  in
  load ();

  file

let is_read_only file =
  match file.loading with
    | None ->
        false
    | Some _ ->
        true

let foreach_view file f =
  List.iter f file.views

let foreach_cursor view f =
  List.iter f view.cursors

(* Move marks after text has been inserted.

   Move marks as if [lines] lines were inserted after [x, y]
   and [characters] characters were inserted after those lines.

   For instance, adding 2 lines and 10 characters means that
   line [y] becomes the beginning of [y] up to [x], plus the first new line;
   a new line is added after that; and 10 characters are added at the beginning
   of the line which was at [y + 1] and is now at [y + 2]. *)
let update_marks_after_insert ~x ~y ~characters ~lines marks =
  let xy = { x; y } in
  let move_mark mark =
    if mark <% xy then
      (* Inserting after mark: do not move mark. *)
      ()
    else (
      (* Inserting before or at mark: move mark. *)
      if mark.y = y then
        (* Inserting on the same line as the mark; x coordinate changes.

           Example 1: inserting XXX at | (i.e. [lines] is 0 and [characters] is 3):

               -------|-------M--------

           Becomes:

               -------|XXX-------M--------

           Example 2: inserting XXX\nYYYY\nZ at | (i.e. [lines] is 2 and [characters] is 1):

               -------|-------M--------

           Becomes:

               -------|XXX\n
               YYYY\n
               Z-------M--------

           Only the length of Z (i.e. [characters]) line matters for the x coordinate,
           not the length of XXX and YYYY. *)
        (
          if lines = 0 then
            mark.x <- mark.x + characters
          else
            mark.x <- mark.x - x + characters;
          mark.y <- mark.y + lines;
        )
      else
        (* Inserting on a previous line; x coordinate does not change. *)
        mark.y <- mark.y + lines
    )
  in
  List.iter move_mark marks

(* Move marks after text has been deleted.

   Move marks as if [lines] lines were deleted after [(x, y)],
   including the end of [y], and [characters] characters were deleted
   after those lines.

   For instance, deleting 2 lines and 10 characters means that
   the end of line [y] is deleted, that line [y + 1] is deleted,
   and that the first 10 characters of line [y + 2] are deleted. *)
let update_marks_after_delete ~x ~y ~characters ~lines marks =
  (* Beginning of the deleted region. *)
  let xy = { x; y } in

  (* End of the deleted region. *)
  let xy2 =
    if lines = 0 then
      { x = x + characters; y }
    else
      { x = characters; y = y + lines }
  in

  let move_mark mark =
    (* For a given mark, either the mark is:
       - before the removed region, in which case the mark does not move;
       - inside the removed region, in which case the mark moves to the beginning of this removed region;
       - after the removed region, in which case the mark moves like when inserting but in reverse. *)
    if mark <% xy then
      (* Deleting after mark: do not move mark. *)
      ()
    else if xy <=% mark && mark <=% xy2 then
      (* Mark is inside deleted region: move mark to the beginning of the deleted region. *)
      (
        mark.x <- x;
        mark.y <- y;
      )
    else (
      (* Mark is after deleted region: move it. *)
      if mark.y = xy2.y then
        (* Deleting on the same line as the mark; x coordinate changes.

           Exemple 1: deleting XXX from a single line (i.e. [lines] is 0 and [characters] is 3):

               -------XXX-----M------

           Becomes:

               ------------M------

           Example 2: deleting XXX\nYYYY\nZ (i.e. [lines] is 2 and [characters] is 1):

               -------XXX\n
               YYYY\n
               Z-----M------

           Becomes:

               ------------M------

           Once again, only the length of the last line matters. *)
        (
          mark.x <- mark.x - characters + (if lines = 0 then 0 else x);
          mark.y <- mark.y - lines;
        )
      else
        (* Deleting lines which are strictly before the mark; x coordinate does not change. *)
        mark.y <- mark.y - lines
    )
  in
  List.iter move_mark marks

let update_all_marks_after_insert ~x ~y ~characters ~lines views =
  let update_view view = update_marks_after_insert ~x ~y ~characters ~lines view.marks in
  List.iter update_view views

let update_all_marks_after_delete ~x ~y ~characters ~lines views =
  let update_view view = update_marks_after_delete ~x ~y ~characters ~lines view.marks in
  List.iter update_view views

let set_text file text =
  if is_read_only file then
    invalid_arg "set_text: file is read-only"
  else (
    file.text <- text;
    file.modified <- true;
  )

let set_cursors view cursors =
  let marks =
    cursors
    |> List.map (fun cursor -> [ cursor.selection_start; cursor.position ])
    |> List.flatten
  in
  view.cursors <- cursors;
  view.marks <- marks

let delete_selection view cursor =
  (* Compute region. *)
  let left, right = selection_boundaries cursor in
  let { x; y } = left in
  let lines, characters =
    if right.y = y then
      (* No line split in selection. *)
      0, right.x - x
    else
      right.y - y, right.x
  in

  (* Delete selection. *)
  set_text view.file (Text.delete_region ~x ~y ~characters ~lines view.file.text);
  update_all_marks_after_delete ~x ~y ~characters ~lines view.file.views

let insert_character (character: Character.t) view cursor =
  set_text view.file (Text.insert_character cursor.position.x cursor.position.y character view.file.text);
  update_all_marks_after_insert ~x: cursor.position.x ~y: cursor.position.y ~characters: 1 ~lines: 0 view.file.views

let insert_new_line view cursor =
  set_text view.file (Text.insert_new_line cursor.position.x cursor.position.y view.file.text);
  update_all_marks_after_insert ~x: cursor.position.x ~y: cursor.position.y ~characters: 0 ~lines: 1 view.file.views

let delete_character view cursor =
  let { x; y } = cursor.position in
  let text = view.file.text in
  let length = Text.get_line_length y text in
  let characters, lines = if x < length then 1, 0 else 0, 1 in
  set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
  update_all_marks_after_delete ~x ~y ~characters ~lines view.file.views

let delete_character_backwards view cursor =
  let { x; y } = cursor.position in
  if x > 0 then
    let text = view.file.text in
    let x = x - 1 in
    let characters = 1 in
    let lines = 0 in
    set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
    update_all_marks_after_delete ~x ~y ~characters ~lines view.file.views
  else if y > 0 then
    let text = view.file.text in
    let y = y - 1 in
    let x = Text.get_line_length y text in
    let characters = 0 in
    let lines = 1 in
    set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
    update_all_marks_after_delete ~x ~y ~characters ~lines view.file.views

let reset_preferred_x file =
  foreach_view file @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  cursor.preferred_x <- cursor.position.x

let edit view f =
  if is_read_only view.file then
    Log.error "buffer is read-only"
  else (
    f view;
    reset_preferred_x view.file
  )

let replace_selection_by_character character view =
  edit view @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_character character view cursor

let replace_selection_by_new_line view =
  edit view @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_new_line view cursor

let delete_selection_or_character view =
  edit view @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character view cursor
  else
    delete_selection view cursor

let delete_selection_or_character_backwards view =
  edit view @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character_backwards view cursor
  else
    delete_selection view cursor
