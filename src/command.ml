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

let move vertical f = fun (state: State.t) ->
  let text = state.focus.view.file.text in
  let move (cursor: Cursors.cursor) =
    let x, y = f text (if vertical then cursor.preferred_x else cursor.x) cursor.y in
    {
      cursor with
        selection_start_x = x;
        selection_start_y = y;
        x;
        y;
        preferred_x = if vertical then cursor.preferred_x else x;
    }
  in
  state.focus.view.cursors <- Cursors.map move state.focus.view.cursors

(******************************************************************************)
(*                                 Definitions                                *)
(******************************************************************************)

let () = define "quit" @@ fun state ->
  raise State.Exit

let () = define "move_right" @@ move false @@ fun text x y ->
  if x >= Text.get_line_length y text then
    if y >= Text.get_line_count text - 1 then
      x, y
    else
      0, y + 1
  else
    x + 1, y

let () = define "move_left" @@ move false @@ fun text x y ->
  if x <= 0 then
    if y <= 0 then
      0, 0
    else
      Text.get_line_length (y - 1) text, y - 1
  else
    x - 1, y

let () = define "move_down" @@ move true @@ fun text x y ->
  if y >= Text.get_line_count text - 1 then
    min x (Text.get_line_length y text), y
  else
    min x (Text.get_line_length (y + 1) text), y + 1

let () = define "move_up" @@ move true @@ fun text x y ->
  if y <= 0 then
    min x (Text.get_line_length y text), y
  else
    min x (Text.get_line_length (y - 1) text), y - 1
