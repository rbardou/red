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
    clipboard: Clipboard.t;
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

type loading =
  | No
  | File of { loaded: int; size: int; sub_strings_rev: (string * int) list }
  | Process of string

type t =
  {
    mutable views: view list;
    mutable text: Text.t;
    mutable modified: bool;
    mutable name: string;
    mutable filename: string option;

    mutable undo_stack: undo list;
    mutable redo_stack: undo list;

    (* If [loading] is [Some (loaded, size, sub_strings_rev)], only [loaded] bytes out of [size]
       have been loaded, and [text] is read-only. *)
    mutable loading: loading;
    mutable spawn_group: Spawn.group option;

    mutable live_process_ids: int list;
    mutable open_file_descriptors: Unix.file_descr list;
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

and undo =
  {
    undo_text: Text.t;
    undo_modified: bool;
    undo_views: undo_view list;
  }

and undo_view =
  {
    undo_view: view;
    undo_scroll_x: int;
    undo_scroll_y: int;
    undo_marks: undo_mark list;
  }

and undo_mark =
  {
    undo_mark: mark;
    undo_x: int;
    undo_y: int;
  }

(* If [file_descr] is in [file.open_file_descriptors], close it. *)
let close_file_descriptor file file_descr =
  let rec find acc list =
    match list with
      | [] ->
          List.rev acc
      | head :: tail ->
          if head = file_descr then
            (
              Unix.close head; (* TODO: error handling *)
              List.rev_append acc tail
            )
          else
            find (head :: acc) tail
  in
  file.open_file_descriptors <- find [] file.open_file_descriptors

(* Wait for [pid] to terminate, in the background. *)
let rec spawn_wait_pid name pid =
  match Unix.waitpid [ WNOHANG ] pid with
    | 0, _ ->
        (* Process is still running. Usually, processes are already dead when their output is closed though. *)
        (* TODO: better way to wait than Spawn.sleep ?? *)
        Spawn.sleep 1. @@ fun () ->
        spawn_wait_pid name pid
    | _, WEXITED code ->
        if name <> "" then Log.info "Process %s exited with code: %d" name code
    | _, WSIGNALED signal ->
        (* TODO: convert signal number *)
        if name <> "" then Log.info "Process %s was killed by signal: %d" name signal
    | _, WSTOPPED signal ->
        (* TODO: convert signal number *)
        if name <> "" then Log.info "Process %s was stopped by signal: %d" name signal

(* If [pid] is in [file.live_process_ids], spawn a process to collect it. *)
let wait_pid name file pid =
  let rec find acc list =
    match list with
      | [] ->
          List.rev acc
      | head :: tail ->
          if head = pid then
            (
              spawn_wait_pid name pid;
              List.rev_append acc tail
            )
          else
            find (head :: acc) tail
  in
  file.live_process_ids <- find [] file.live_process_ids

let close_all_file_descriptors file =
  List.iter Unix.close file.open_file_descriptors; (* TODO: error handling *)
  file.open_file_descriptors <- []

let wait_all_pids file =
  List.iter (spawn_wait_pid "") file.live_process_ids; (* TODO: error handling *)
  file.live_process_ids <- []

let create_cursor x y =
  {
    selection_start = { x; y };
    position = { x; y };
    preferred_x = x;
    clipboard = { text = Text.empty };
  }

let foreach_view file f =
  List.iter f file.views

let foreach_cursor view f =
  List.iter f view.cursors

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

(* You may want to use [State.create_file] instead. *)
let create name text =
  {
    views = [];
    text;
    modified = false;
    name;
    filename = None;
    loading = No;
    undo_stack = [];
    redo_stack = [];
    spawn_group = None;
    live_process_ids = [];
    open_file_descriptors = [];
  }

let kill_spawn_group file =
  match file.spawn_group with
    | None ->
        ()
    | Some group ->
        Spawn.kill group;
        file.spawn_group <- None

(* Reset a file to an empty text as if it was created by [create].
   Keep views, but reset them as well. *)
let reset file =
  (* Reset file. *)
  file.text <- Text.empty;
  file.filename <- None;
  file.loading <- No;
  file.modified <- false;
  file.undo_stack <- [];
  file.redo_stack <- [];
  kill_spawn_group file;
  close_all_file_descriptors file;
  wait_all_pids file;

  (* Reset views. *)
  foreach_view file @@ fun view ->
  view.scroll_x <- 0;
  view.scroll_y <- 0;
  let cursor = create_cursor 0 0 in
  view.marks <- [ cursor.selection_start; cursor.position ];
  view.cursors <- [ cursor ]

let create_spawn_group file =
  kill_spawn_group file;
  let group = Spawn.group () in
  file.spawn_group <- Some group;
  group

let load file filename =
  let file_descr =
    (* TODO: error handling (e.g. file does not exist) *)
    Unix.openfile filename [ O_RDONLY ] 0o640
  in
  let size =
    (* TODO: error handling *)
    (Unix.fstat file_descr).st_size
  in
  reset file;
  file.filename <- Some filename;
  file.loading <- File { loaded = 0; size; sub_strings_rev = [] };
  file.open_file_descriptors <- file_descr :: file.open_file_descriptors;
  let group = create_spawn_group file in

  (* Start loading. *)
  let rec load () =
    Spawn.on_read ~group file_descr @@ fun () ->
    match file.loading with
      | No | Process _ ->
          assert false (* did someone else temper with this file? *)
      | File { loaded; size; sub_strings_rev } ->
          let bytes = Bytes.create 8192 in
          let len = Unix.read file_descr bytes 0 8192 in
          if len = 0 then
            (* TODO: [Text.of_utf8_substrings_offset_0] can block for a while.
               Replace it by some kind of iterative parser. *)
            let text = Text.of_utf8_substrings_offset_0 (List.rev sub_strings_rev) in
            file.text <- text;
            close_file_descriptor file file_descr;
            file.loading <- No
          else
            let sub_strings_rev = (Bytes.unsafe_to_string bytes, len) :: sub_strings_rev in
            file.loading <- File { loaded = loaded + len; size; sub_strings_rev };
            load ()
  in
  load ()

let create_process file program arguments =
  reset file;
  file.filename <- None;
  file.loading <- Process program;
  let group = create_spawn_group file in

  (* Create process. *)
  (* TODO: error handling for all Unix commands. *)
  let in_exit, in_entry = Unix.pipe () in
  let out_exit, out_entry = Unix.pipe () in
  Unix.set_close_on_exec in_entry;
  Unix.set_close_on_exec out_exit;
  let pid =
    Unix.create_process
      program
      (Array.of_list (program :: arguments))
      in_exit
      out_entry
      out_entry
  in
  Unix.close in_exit;
  Unix.close out_entry;
  Unix.close in_entry; (* We won't send inputs to the process. *)
  file.open_file_descriptors <- out_exit :: file.open_file_descriptors;
  file.live_process_ids <- pid :: file.live_process_ids;
  Log.info "Created process: %s" program;

  (* Start reading. *)
  let rec read alive file_descr =
    Spawn.on_read ~group file_descr @@ fun () ->
    let bytes = Bytes.create 8192 in
    let len = Unix.read file_descr bytes 0 8192 in
    Log.info "Output from %s: %S" program (Bytes.sub_string bytes 0 len); (* TODO: put that in file.text *)
    if len = 0 then
      (
        file.loading <- No;
        close_file_descriptor file file_descr;
        wait_pid program file pid
      )
    else
      read alive file_descr
  in
  read true out_exit

let is_read_only file =
  match file.loading with
    | No ->
        false
    | File _ | Process _ ->
        true

(* Iterate on cursors and their clipboards.
   If there is only one cursor, use global clipboard instead of cursor clipboard. *)
let foreach_cursor_clipboard (global_clipboard: Clipboard.t) view f =
  match view.cursors with
    | [ cursor ] ->
        f global_clipboard cursor
    | cursors ->
        List.iter (fun cursor -> f cursor.clipboard cursor) cursors

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

let make_undo file =
  let make_undo_mark mark =
    {
      undo_mark = mark;
      undo_x = mark.x;
      undo_y = mark.y;
    }
  in
  let make_undo_view view =
    {
      undo_view = view;
      undo_scroll_x = view.scroll_x;
      undo_scroll_y = view.scroll_y;
      undo_marks = List.map make_undo_mark view.marks;
    }
  in
  {
    undo_text = file.text;
    undo_modified = file.modified;
    undo_views = List.map make_undo_view file.views;
  }

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

let edit save_undo file f =
  if is_read_only file then
    Log.error "buffer is read-only"
  else (
    if save_undo then
      let undo = make_undo file in
      f ();
      file.undo_stack <- undo :: file.undo_stack;
      file.redo_stack <- []
    else
      f ();
    reset_preferred_x file;
  )

let replace_selection_by_character character view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_character character view cursor

let replace_selection_by_new_line view =
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_new_line view cursor

let delete_selection_or_character view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character view cursor
  else
    delete_selection view cursor

let delete_selection_or_character_backwards view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character_backwards view cursor
  else
    delete_selection view cursor

let delete_end_of_line view =
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  let text = view.file.text in
  let x = cursor.position.x in
  let y = cursor.position.y in
  let characters, lines =
    let length = Text.get_line_length y text in
    if x >= length then
      0, 1
    else
      length - x, 0
  in
  set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
  update_all_marks_after_delete ~x ~y ~characters ~lines view.file.views

let get_selected_text text cursor =
  let left, right = selection_boundaries cursor in
  (* The cursor itself is not included in the selection, hence the value of x2.
     A negative value here is not an issue for Text.sub. *)
  Text.sub ~x1: left.x ~y1: left.y ~x2: (right.x - 1) ~y2: right.y text

let copy (global_clipboard: Clipboard.t) view =
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->
  clipboard.text <- get_selected_text view.file.text cursor

let cut (global_clipboard: Clipboard.t) view =
  edit true view.file @@ fun () ->
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->
  clipboard.text <- get_selected_text view.file.text cursor;
  delete_selection view cursor

let paste (global_clipboard: Clipboard.t) view =
  edit true view.file @@ fun () ->
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->

  (* Replace selection with clipboard. *)
  delete_selection view cursor;
  let x = cursor.position.x in
  let y = cursor.position.y in
  let sub = clipboard.text in
  set_text view.file (Text.insert_text ~x ~y ~sub view.file.text);

  (* Update marks. *)
  let lines = Text.get_line_count sub - 1 in
  let characters = Text.get_line_length lines sub in
  update_all_marks_after_insert ~x ~y ~characters ~lines view.file.views

let restore_undo_point file undo =
  file.text <- undo.undo_text;
  file.modified <- undo.undo_modified;
  let undo_mark undo =
    undo.undo_mark.x <- undo.undo_x;
    undo.undo_mark.y <- undo.undo_y;
  in
  let undo_view undo =
    undo.undo_view.scroll_x <- undo.undo_scroll_x;
    undo.undo_view.scroll_y <- undo.undo_scroll_y;
    List.iter undo_mark undo.undo_marks
  in
  List.iter undo_view undo.undo_views

let undo file =
  edit false file @@ fun () ->
  match file.undo_stack with
    | [] ->
        Log.info "Nothing to undo."
    | undo :: remaining_stack ->
        Log.info "Undo.";
        file.undo_stack <- remaining_stack;
        file.redo_stack <- make_undo file :: file.redo_stack;
        restore_undo_point file undo

let redo file =
  edit false file @@ fun () ->
  match file.redo_stack with
    | [] ->
        Log.info "Nothing to redo."
    | undo :: remaining_stack ->
        Log.info "Redo.";
        file.undo_stack <- make_undo file :: file.undo_stack;
        file.redo_stack <- remaining_stack;
        restore_undo_point file undo
