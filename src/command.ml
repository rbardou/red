module String_map = Map.Make (String)

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
  File.foreach_cursor state.focus.view move

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

(******************************************************************************)
(*                                 Definitions                                *)
(******************************************************************************)

let () = define "quit" @@ fun state ->
  raise State.Exit

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
