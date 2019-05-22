module String_map = Map.Make (String)

let commands = ref String_map.empty

let define name (f: State.t -> unit) =
  commands := String_map.add name f !commands

type context =
  | Global
  | File
  | Prompt
  | List_choice

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
    | List_choice -> state.list_choice_bindings <- Key.Map.add key command state.list_choice_bindings

(******************************************************************************)
(*                                   Helpers                                  *)
(******************************************************************************)

let abort ?exn x = State.abort ?exn x

let rec find_character_forwards text x y f =
  if y >= Text.get_line_count text then
    None
  else
    match Text.get x y text with
      | None ->
          if f "\n" then
            Some (x, y)
          else
            find_character_forwards text 0 (y + 1) f
      | Some character ->
          if f character then
            Some (x, y)
          else
            find_character_forwards text (x + 1) y f

let rec find_character_backwards text x y f =
  if y < 0 then
    None
  else if x < 0 then
    find_character_backwards text (Text.get_line_length (y - 1) text) (y - 1) f
  else
    match Text.get x y text with
      | None ->
          if f "\n" then
            Some (x, y)
          else
            find_character_backwards text (x - 1) y f
      | Some character ->
          if f character then
            Some (x, y)
          else
            find_character_backwards text (x - 1) y f

let rec find_line_forwards text y f =
  if y >= Text.get_line_count text then
    None
  else
    if f (Text.get_line y text) then
      Some y
    else
      find_line_forwards text (y + 1) f

let rec find_line_backwards text y f =
  if y < 0 then
    None
  else
    if f (Text.get_line y text) then
      Some y
    else
      find_line_backwards text (y - 1) f

(* Change cursor position (apply [f] to get new coordinates).
   Update [preferred_x] unless [vertical].
   Reset selection if [reset_selection]. *)
let move_cursor reset_selection vertical (view: File.view) (cursor: File.cursor) f =
  let position = cursor.position in
  let x, y = f view.file.text (if vertical then cursor.preferred_x else position.x) position.y in
  if reset_selection then (
    let selection_start = cursor.selection_start in
    selection_start.x <- x;
    selection_start.y <- y;
  );
  position.x <- x;
  position.y <- y;
  cursor.preferred_x <- if vertical then cursor.preferred_x else x

(* Change cursor position (apply [f] to get new coordinates).
   Update [preferred_x] unless [vertical].
   Reset selection if [reset_selection]. *)
let move reset_selection vertical f (state: State.t) =
  let view = state.focus.view in
  (
    File.foreach_cursor view @@ fun cursor ->
    move_cursor reset_selection vertical view cursor f
  );
  File.recenter_if_needed view

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

let move_right_word text x y =
  match find_character_forwards text x y Character.is_word_character with
    | None ->
        move_end_of_file text x y
    | Some (x, y) ->
        match find_character_forwards text x y Character.is_not_word_character with
          | None ->
              move_end_of_file text x y
          | Some (x, y) ->
              x, y

let move_left_word text x y =
  (* Move left once to avoid staying at the same place if we are already at the beginning of a word. *)
  let x, y = move_left text x y in
  match find_character_backwards text x y Character.is_word_character with
    | None ->
        0, 0
    | Some (x, y) ->
        match find_character_backwards text x y Character.is_not_word_character with
          | None ->
              0, 0
          | Some (x, y) ->
              (* We are at just before the word, go right once to be at the beginning of the word. *)
              move_right text x y

let move_down_paragraph text x y =
  match find_line_forwards text y Line.is_not_empty with
    | None ->
        move_end_of_file text x y
    | Some y ->
        match find_line_forwards text y Line.is_empty with
          | None ->
              move_end_of_file text x y
          | Some y ->
              0, y

let move_up_paragraph text x y =
  match find_line_backwards text y Line.is_not_empty with
    | None ->
        0, 0
    | Some y ->
        match find_line_backwards text y Line.is_empty with
          | None ->
              0, 0
          | Some y ->
              0, y

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
  File.if_only_one_cursor view @@ fun cursor ->
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
        let perm =
          match Unix.stat filename with
            | exception Unix.Unix_error _ ->
                None
            | stat ->
                Some stat.st_perm
        in
        let temporary_filename = System.find_temporary_filename filename in
        Text.output_file ?perm temporary_filename file.text;
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
    let validate string =
      if global && Layout.panel_is_visible initial_focus state.layout then State.set_focus state initial_focus;
      validate string
    in
    let file = File.create prompt (Text.one_line (Line.of_utf8_string default)) in
    let view = File.create_view (Prompt { prompt; validate }) file in
    select_all view;
    Panel.create view
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

let choose_from_list ?(default = "") (prompt: string) (choices: string list) (state: State.t)
    (validate: string -> unit) =
  (* Create choice file and view. *)
  let choice_view =
    let file = File.create prompt (Text.one_line (Line.of_utf8_string default)) in
    let view =
      let original_view = state.focus.view in
      File.create_view (List_choice { prompt; validate; choices; choice = 0; original_view }) file
    in
    select_all view;
    view
  in

  (* Replace current view with choice view. *)
  state.focus.view <- choice_view

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
    let view = File.create_view File file in
    panel.view <- view
  )

let () = define "new" @@ fun state ->
  let panel = state.focus in
  let file = State.create_file state "(new file)" Text.empty in
  let view = File.create_view File file in
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
let () = define "move_right_word" @@ move true false move_right_word
let () = define "move_left_word" @@ move true false move_left_word
let () = define "move_down_paragraph" @@ move true false move_down_paragraph
let () = define "move_up_paragraph" @@ move true false move_up_paragraph

let () = define "select_right" @@ move false false move_right
let () = define "select_left" @@ move false false move_left
let () = define "select_down" @@ move false true move_down
let () = define "select_up" @@ move false true move_up
let () = define "select_end_of_line" @@ move false false move_end_of_line
let () = define "select_beginning_of_line" @@ move false false move_beginning_of_line
let () = define "select_end_of_file" @@ move false false move_end_of_file
let () = define "select_beginning_of_file" @@ move false false move_beginning_of_file
let () = define "select_right_word" @@ move false false move_right_word
let () = define "select_left_word" @@ move false false move_left_word
let () = define "select_down_paragraph" @@ move false false move_down_paragraph
let () = define "select_up_paragraph" @@ move false false move_up_paragraph

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
  let view = state.focus.view in
  File.replace_selection_by_new_line view;
  File.recenter_if_needed view

let () = define "delete_character" @@ fun state ->
  let view = state.focus.view in
  File.delete_selection_or_character view;
  File.recenter_if_needed view

let () = define "delete_character_backwards" @@ fun state ->
  let view = state.focus.view in
  File.delete_selection_or_character_backwards view;
  File.recenter_if_needed view

let () = define "delete_end_of_line" @@ fun state ->
  let view = state.focus.view in
  (
    File.delete_from_cursors view @@ fun text cursor ->
    (* Cannot just use [move_end_of_line] here because we want to delete the \n if we are at the end of the line. *)
    let x = cursor.position.x in
    let y = cursor.position.y in
    let length = Text.get_line_length y text in
    if x >= length then
      0, y + 1
    else
      length, y
  );
  File.recenter_if_needed view

let () = define "delete_end_of_word" @@ fun state ->
  let view = state.focus.view in
  (
    File.delete_from_cursors view @@ fun text cursor ->
    move_right_word text cursor.position.x cursor.position.y
  );
  File.recenter_if_needed view

let () = define "delete_beginning_of_word" @@ fun state ->
  let view = state.focus.view in
  (
    File.delete_from_cursors view @@ fun text cursor ->
    move_left_word text cursor.position.x cursor.position.y
  );
  File.recenter_if_needed view

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

let () = define "cut" @@ fun state ->
  let view = state.focus.view in
  File.cut state.clipboard view;
  File.recenter_if_needed view

let () = define "paste" @@ fun state ->
  let view = state.focus.view in
  File.paste state.clipboard view;
  File.recenter_if_needed view

let () = define "undo" @@ fun state -> File.undo state.focus.view.file
let () = define "redo" @@ fun state -> File.redo state.focus.view.file

let () = define "validate" @@ fun state ->
  let panel = state.focus in
  match panel.view.kind with
    | Prompt { validate } ->
        State.remove_panel panel state;
        validate (Text.to_string panel.view.file.text)
    | List_choice { validate; original_view; choice; choices } ->
        let filter = Text.to_string panel.view.file.text in
        panel.view <- original_view;
        (
          match List.nth (Panel.filter_choices filter choices) choice with
            | exception Failure _ ->
                validate filter
            | choice ->
                validate choice
        )
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
  match Shell_lexer.items [] [] (Lexing.from_string command) with
    | exception Failure reason ->
        abort "parse error in command: %s" reason
    | [] ->
        ()
    | program :: arguments ->
        let file = State.create_file state ("<" ^ command ^ ">") Text.empty in
        panel.view <- File.create_view File file;
        File.create_process file program arguments

let () = define "switch_file" @@ fun state ->
  let panel = state.focus in
  let compare_names a b =
    (* By using uppercase instead of lowercase, symbols like _ are higher in the choice list. *)
    String.compare (String.uppercase_ascii a) (String.uppercase_ascii b)
  in
  let names = List.sort compare_names (List.map File.get_name state.files) in
  choose_from_list "Switch to file: " names state @@ fun choice ->
  match List.find (File.has_name choice) state.files with
    | exception Not_found ->
        abort "no such file: %s" choice
    | file ->
        (* TODO: use previous view that this panel had *)
        let view =
          match file.views with
            | [] ->
                File.create_view File file
            | head :: _ ->
                head
        in
        panel.view <- view

let () = define "choose_next" @@ fun state ->
  match state.focus.view.kind with
    | List_choice choice ->
        let choices = Panel.filter_choices (Text.to_string state.focus.view.file.text) choice.choices in
        choice.choice <- choice.choice + 1;
        let max_choice = List.length choices - 1 in
        if choice.choice > max_choice then choice.choice <- 0
    | _ ->
        abort "focused panel is not a prompt"

let () = define "choose_previous" @@ fun state ->
  match state.focus.view.kind with
    | List_choice choice ->
        let choices = Panel.filter_choices (Text.to_string state.focus.view.file.text) choice.choices in
        choice.choice <- choice.choice - 1;
        if choice.choice < 0 then choice.choice <- List.length choices - 1;
        if choice.choice < 0 then choice.choice <- 0
    | _ ->
        abort "focused panel is not a prompt"
