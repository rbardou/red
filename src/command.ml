module String_map = Map.Make (String)

let commands = ref String_map.empty

let define name (f: State.t -> unit) =
  commands := String_map.add name f !commands

type context =
  | Global
  | File
  | Prompt

let bind context (state: State.t) key name =
  let command =
    match String_map.find name !commands with
      | exception Not_found ->
          Log.error "unbound command: %s" name;
          (fun _ -> ())
      | command ->
          command
  in
  match context with
    | Global -> state.global_bindings <- Key.Map.add key command state.global_bindings
    | File -> state.file_bindings <- Key.Map.add key command state.file_bindings
    | Prompt -> state.prompt_bindings <- Key.Map.add key command state.prompt_bindings

(******************************************************************************)
(*                                   Helpers                                  *)
(******************************************************************************)

let abort ?exn x = State.abort ?exn x

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
        State.set_focus state panel

let move_after_scroll (view: File.view) old_scroll =
  let delta = view.scroll_y - old_scroll in
  let text = view.file.text in
  let max_y = Text.get_line_count text - 1 in
  if_only_one_cursor view @@ fun cursor ->
  cursor.position.y <- max 0 (min max_y (cursor.position.y + delta));
  cursor.position.x <- min (Text.get_line_length cursor.position.y text) cursor.preferred_x;
  cursor.selection_start.x <- cursor.position.x;
  cursor.selection_start.y <- cursor.position.y

let select_all view =
  File.foreach_cursor view @@ fun cursor ->
  cursor.selection_start.x <- 0;
  cursor.selection_start.y <- 0;
  let text = view.file.text in
  let max_y = Text.get_line_count text - 1 in
  cursor.position.y <- max_y;
  cursor.position.x <- Text.get_line_length max_y text

let save (file: File.t) filename =
  if filename <> "" then (
    file.name <- filename;
    file.filename <- Some filename;

    if not (System.file_exists filename) then
      (
        Text.output_file filename file.text;
        Log.info "Wrote: %s" filename
      )
    else
      (
        let temporary_filename = System.find_temporary_filename filename in
        Text.output_file temporary_filename file.text;
        Log.info "Wrote: %s" temporary_filename;
        System.move_file temporary_filename filename;
        Log.info "Moved to: %s" filename
      );

    file.modified <- false;

    (* Clear undo stack (at the very least we should set the modified flags in all undo points). *)
    file.undo_stack <- [];
    file.redo_stack <- [];
  )

let prompt ?(global = false) ?(default = "") (prompt: string) (state: State.t) (validate: string -> unit) =
  (* Create prompt panel. *)
  let prompt_panel =
    let initial_focus = state.focus in
    let file = File.create prompt (Text.one_line (Line.of_utf8_string default)) in
    let view = File.create_view file in
    select_all view;
    let validate text =
      if global && Layout.panel_is_visible initial_focus state.layout then State.set_focus state initial_focus;
      validate (Text.to_string text)
    in
    Panel.create (Prompt { prompt; validate }) view
  in

  (* Add panel to layout and focus it. *)
  let new_layout =
    if global then
      (* Split the whole layout. *)
      Layout.split Vertical ~pos: (Absolute_second 1) ~main: Second
        state.layout (Layout.single prompt_panel)
    else
      (* Split focused panel. *)
      let new_sublayout =
        Layout.split Vertical ~pos: (Absolute_second 1) ~main: Second
          (Layout.single state.focus) (Layout.single prompt_panel)
      in
      match Layout.replace_panel state.focus new_sublayout state.layout with
        | None ->
            abort "focused panel is not in current layout"
        | Some new_layout ->
            new_layout
  in
  State.set_layout state new_layout;
  State.set_focus state prompt_panel

let rec prompt_confirmation ?global ?(repeated = false) message state confirm =
  let message = if repeated then "Please answer yes or no. " ^ message else message in
  prompt ?global ~default: "no" message state @@ fun response ->
  if String.lowercase_ascii response = "yes" then
    confirm ()
  else if String.lowercase_ascii response = "no" then
    ()
  else
    prompt_confirmation ?global ~repeated: true message state confirm

(******************************************************************************)
(*                                 Definitions                                *)
(******************************************************************************)

let () = define "quit" @@ fun state ->
  let modified_files = List.filter (fun (file: File.t) -> file.modified) state.files in
  let modified_file_count = List.length modified_files in
  if modified_file_count = 0 then
    raise State.Exit
  else
    let message =
      if modified_file_count = 1 then
        "There is 1 modified file, really exit? "
      else
        "There are " ^ string_of_int modified_file_count ^ " modified file(s), really exit? "
    in
    prompt_confirmation ~global: true message state @@ fun () ->
    raise State.Exit

let () = define "save" @@ fun state ->
  let file_to_save = state.focus.view.file in
  match file_to_save.filename with
    | None ->
        prompt ?default: file_to_save.filename "Save as: " state (save file_to_save)
    | Some filename ->
        save file_to_save filename

let () = define "save_as" @@ fun state ->
  let file_to_save = state.focus.view.file in
  prompt ?default: file_to_save.filename "Save as: " state (save file_to_save)

let () = define "open" @@ fun state ->
  let panel = state.focus in
  prompt "Open file: " state @@ fun filename ->
  if filename <> "" then (
    if not (System.file_exists filename) then abort "file does not exist: %S" filename;
    let file = State.create_file_loading state filename in
    let view = File.create_view file in
    panel.view <- view
  )

let () = define "new" @@ fun state ->
  let panel = state.focus in
  let file = State.create_file state "(new file)" Text.empty in
  let view = File.create_view file in
  panel.view <- view

let () = define "remove_panel" @@ fun state -> State.remove_panel state.focus state

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

let () = define "select_all" @@ fun state ->
  select_all state.focus.view

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

let () = define "delete_end_of_line" @@ fun state ->
  File.delete_end_of_line state.focus.view;
  recenter_if_needed state

let () = define "create_cursors_from_selection" @@ fun state ->
  let view = state.focus.view in
  match view.cursors with
    | [] ->
        ()
    | cursor :: _ :: _ ->
        view.cursors <- [ cursor ]
    | [ cursor ] ->
        let first, last, reverse =
          let sel_y = cursor.selection_start.y in
          let cur_y = cursor.position.y in
          if sel_y <= cur_y then
            sel_y, cur_y, true
          else
            cur_y, sel_y, false
        in
        let rec range acc first last =
          if last < first then
            acc
          else
            range (last :: acc) first (last - 1)
        in
        let text = view.file.text in
        let create_cursor y = File.create_cursor (min (Text.get_line_length y text) cursor.position.x) y in
        let cursors = List.map create_cursor (range [] first last) in
        let cursors = if reverse then List.rev cursors else cursors in
        File.set_cursors view cursors

let () = define "copy" @@ fun state -> File.copy state.clipboard state.focus.view
let () = define "cut" @@ fun state -> File.cut state.clipboard state.focus.view
let () = define "paste" @@ fun state -> File.paste state.clipboard state.focus.view

let () = define "undo" @@ fun state -> File.undo state.focus.view.file
let () = define "redo" @@ fun state -> File.redo state.focus.view.file

let () = define "validate" @@ fun state ->
  let panel = state.focus in
  match panel.kind with
    | Prompt { validate } ->
        State.remove_panel panel state;
        validate panel.view.file.text
    | _ ->
        abort "focused panel is not a prompt"

let () = define "execute_command" @@ fun state ->
  prompt "Execute command: " state @@ fun name ->
  match String_map.find name !commands with
    | exception Not_found ->
        abort "unbound command: %s" name
    | command ->
        command state

let () = define "execute_process" @@ fun state ->
  let panel = state.focus in
  prompt "Execute process: " state @@ fun command ->
  (* TODO: parse arguments *)
  if command <> "" then (
    let file = File.create ("<" ^ command ^ ">") Text.empty in
    panel.view <- File.create_view file;
    File.create_process file command []
  )
