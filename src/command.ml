module String_map = Map.Make (String)

module Int =
struct
  type t = int
  let compare = (Pervasives.compare: int -> int -> int)
end

module Int_set = Set.Make (Int)

let commands = ref String_map.empty

let define name (f: State.t -> unit) =
  commands := String_map.add name f !commands

let bind (state: State.t) key name =
  match String_map.find name !commands with
    | exception Not_found ->
        Log.errorf "unbound command: %s" name
    | command ->
        state.bindings <- Key.Map.add key command state.bindings

(******************************************************************************)
(*                                   Helpers                                  *)
(******************************************************************************)

let recenter_y (view: File.view) (cursor: File.cursor) =
  view.scroll_y <- max 0 (cursor.position.y - view.height / 2)

let recenter_x (view: File.view) (cursor: File.cursor) =
  view.scroll_x <- max 0 (cursor.position.x - view.width / 2)

let if_only_one_cursor (view: File.view) f =
  match view.cursors with
    | [ cursor ] ->
        f cursor
    | _ ->
        ()

let recenter_if_needed (state: State.t) =
  let view = state.focus.view in
  if_only_one_cursor view @@ fun cursor ->
  if cursor.position.x < view.scroll_x || cursor.position.x >= view.scroll_x + view.width - 1 then
    recenter_x view cursor;
  if cursor.position.y < view.scroll_y || cursor.position.y >= view.scroll_y + view.height - 1 then
    recenter_y view cursor

(* Change cursor position (apply [f] to get new coordinates).
   Update [preferred_x] unless [vertical].
   Reset selection if [reset_selection]. *)
let move reset_selection vertical f (state: State.t) =
  let text = state.focus.view.file.text in
  let move (cursor: File.cursor) =
    let position = cursor.position in
    let x, y = f text (if vertical then cursor.preferred_x else position.x) position.y in
    if reset_selection then (
      let selection_start = cursor.selection_start in
      selection_start.x <- x;
      selection_start.y <- y;
    );
    position.x <- x;
    position.y <- y;
    cursor.preferred_x <- if vertical then cursor.preferred_x else x;
  in
  File.foreach_cursor state.focus.view move;
  recenter_if_needed state

let move_right text x y =
  if x >= Text.get_line_length y text then
    if y >= Text.get_line_count text - 1 then
      x, y
    else
      0, y + 1
  else
    x + 1, y

let move_left text x y =
  if x <= 0 then
    if y <= 0 then
      0, 0
    else
      Text.get_line_length (y - 1) text, y - 1
  else
    x - 1, y

let move_down text x y =
  if y >= Text.get_line_count text - 1 then
    min x (Text.get_line_length y text), y
  else
    min x (Text.get_line_length (y + 1) text), y + 1

let move_up text x y =
  if y <= 0 then
    min x (Text.get_line_length y text), y
  else
    min x (Text.get_line_length (y - 1) text), y - 1

let move_end_of_line text _ y =
  Text.get_line_length y text, y

let move_beginning_of_line text _ y =
  0, y

let move_end_of_file text _ _ =
  let y = Text.get_line_count text - 1 in
  Text.get_line_length y text, y

let move_beginning_of_file text _ _ =
  0, 0

let focus_relative get (state: State.t) =
  match get state.focus state.layout with
    | None ->
        ()
    | Some panel ->
        state.focus <- panel

let move_after_scroll (view: File.view) old_scroll =
  let delta = view.scroll_y - old_scroll in
  let text = view.file.text in
  let max_y = Text.get_line_count text - 1 in
  if_only_one_cursor view @@ fun cursor ->
  cursor.position.y <- max 0 (min max_y (cursor.position.y + delta));
  cursor.position.x <- min (Text.get_line_length cursor.position.y text) cursor.preferred_x;
  cursor.selection_start.x <- cursor.position.x;
  cursor.selection_start.y <- cursor.position.y

(******************************************************************************)
(*                                 Definitions                                *)
(******************************************************************************)

let () = define "quit" @@ fun state -> raise State.Exit

let () = define "save" @@ fun state ->
  let file = state.focus.view.file in
  (* TODO: choose a temporary filename, and move the file after that *)
  match file.filename with
    | None ->
        Log.info "TODO: prompt for file name"
    | Some filename ->
        match Text.output_file filename file.text with
          | Ok () ->
              file.modified <- false;
              Log.infof "Wrote: %s" filename
          | Error exn ->
              Log.error (Printexc.to_string exn)

let () = define "move_right" @@ move true false move_right
let () = define "move_left" @@ move true false move_left
let () = define "move_down" @@ move true true move_down
let () = define "move_up" @@ move true true move_up
let () = define "move_end_of_line" @@ move true false move_end_of_line
let () = define "move_beginning_of_line" @@ move true false move_beginning_of_line
let () = define "move_end_of_file" @@ move true false move_end_of_file
let () = define "move_beginning_of_file" @@ move true false move_beginning_of_file

let () = define "select_right" @@ move false false move_right
let () = define "select_left" @@ move false false move_left
let () = define "select_down" @@ move false true move_down
let () = define "select_up" @@ move false true move_up
let () = define "select_end_of_line" @@ move false false move_end_of_line
let () = define "select_beginning_of_line" @@ move false false move_beginning_of_line
let () = define "select_end_of_file" @@ move false false move_end_of_file
let () = define "select_beginning_of_file" @@ move false false move_beginning_of_file

let () = define "focus_right" @@ focus_relative Layout.get_panel_right
let () = define "focus_left" @@ focus_relative Layout.get_panel_left
let () = define "focus_down" @@ focus_relative Layout.get_panel_down
let () = define "focus_up" @@ focus_relative Layout.get_panel_up

let () = define "scroll_down" @@ fun state ->
  let view = state.focus.view in
  let text = view.file.text in

  (* Scroll. *)
  let max_y = Text.get_line_count text - 1 in
  let max_scroll =
    (* Last line is often empty, so we want to see at least the line before that, unless panel height is 1. *)
    if view.height <= 1 then max_y else max_y - 1
  in
  let old_scroll = view.scroll_y in
  view.scroll_y <- min max_scroll (view.scroll_y + view.height / 2);

  (* Move cursor. *)
  move_after_scroll view old_scroll

let () = define "scroll_up" @@ fun state ->
  let view = state.focus.view in

  (* Scroll. *)
  let old_scroll = view.scroll_y in
  view.scroll_y <- max 0 (view.scroll_y - view.height / 2);

  (* Move cursor. *)
  move_after_scroll view old_scroll

let () = define "insert_new_line" @@ fun state ->
  File.replace_selection_by_new_line state.focus.view;
  recenter_if_needed state

let () = define "delete_character" @@ fun state ->
  File.delete_selection_or_character state.focus.view;
  recenter_if_needed state

let () = define "delete_character_backwards" @@ fun state ->
  File.delete_selection_or_character_backwards state.focus.view;
  recenter_if_needed state

let () = define "create_cursors_from_selection" @@ fun state ->
  let view = state.focus.view in

  (* Compute the list of y coordinates. *)
  let rec gather_cursors lines (cursor: File.cursor) =
    let rec gather_lines lines start finish =
      if finish < start then
        lines
      else
        let lines = Int_set.add finish lines in
        gather_lines lines start (finish - 1)
    in
    let left, right = File.selection_boundaries cursor in
    gather_lines lines left.y right.y
  in
  let lines =
    view.cursors
    |> List.fold_left gather_cursors Int_set.empty
    |> Int_set.elements
  in

  (* Replace cursors with new cursors. *)
  File.set_cursors view (List.map (File.create_cursor 0) lines)
