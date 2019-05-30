open Misc

module H = Help

(* Built-in command definitions. *)
type command =
  {
    name: string;
    help: (Help.maker -> unit) option;
    partial: Redl.Typing.command Redl.Typing.partial;
  }

(* List of all built-in commands. *)
let commands: command list ref = ref []

type _ typ =
  | Command: Redl.Typing.command typ
  | Function: { parameter_name: string; parameter_type: ('p, 'p) Redl.Typing.typ; result: 'r typ } -> ('p -> 'r) typ

(* Make a parameterized command type. *)
let (@->) (parameter_name, parameter_type) result =
  Function { parameter_name; parameter_type; result }

let (-:) name (typ: (_, _) Redl.Typing.typ) =
  name, typ

(* Add a command, parameterized or not, to the list of built-in commands. *)
let define (type f) name ?help (typ: f typ) (f: f) =
  (* Declare command. *)
  let rec make_type: type f. f typ -> (f, Redl.Typing.command) Redl.Typing.typ = function
    | Command ->
        Command
    | Function { parameter_type; result } ->
        Function (parameter_type, make_type result)
  in
  let command =
    {
      name;
      help;
      partial = Partial { typ = make_type typ; value = Constant f };
    }
  in
  commands := command :: !commands;

  (* Declare help page. *)
  let rec get_parameters: type f. _ -> f typ -> _ = fun acc typ ->
    match typ with
      | Command ->
          List.rev acc
      | Function { parameter_name; parameter_type; result } ->
          get_parameters ((parameter_name, Redl.Typing.Type parameter_type) :: acc) result
  in
  Help.overload_command name ?help (get_parameters [] typ)

let setup_initial_env overload_command =
  let add command = overload_command command.name command.partial in
  List.iter add !commands

let bind (context: State.Context.t) (state: State.t) (key: Key.t) (command: string) =
  let ast = Redl.parse_string command in
  let command =
    (* Parse, type, but do not execute yet. *)
    state.run_string command
  in
  let context_bindings = State.get_context_bindings context state in
  let context_bindings = Key.Map.add key command context_bindings in
  state.bindings <- State.Context_map.add context context_bindings state.bindings;
  Help.bind context key ast

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
    let file = File.create prompt (Text.of_utf8_string default) in
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

let choose_from_list ?(default = "") ?(choice = -1) (prompt: string) (choices: string list) (state: State.t)
    (validate: string -> unit) =
  (* Create choice file and view. *)
  let choice_view =
    let file = File.create prompt (Text.of_utf8_string default) in
    let view =
      let original_view = state.focus.view in
      File.create_view (List_choice { prompt; validate; choices; choice; original_view }) file
    in
    select_all view;
    view
  in

  (* Replace current view with choice view. *)
  state.focus.view <- choice_view

let sort_names list =
  let compare_names a b =
    (* By using uppercase instead of lowercase, symbols like _ are higher in the choice list. *)
    String.compare (String.uppercase_ascii a) (String.uppercase_ascii b)
  in
  List.sort compare_names list

let choose_from_file_system ?default (prompt: string) (state: State.t) (validate: string -> unit) =
  let append_dir_sep_if_needed filename =
    let length = String.length filename in
    let sep_length = String.length Filename.dir_sep in
    if
      length < sep_length ||
      String.sub filename (length - sep_length) sep_length <> Filename.dir_sep
    then
      filename ^ Filename.dir_sep
    else
      filename
  in

  let starting_directory, default =
    match default with
      | None ->
          System.get_cwd (), None
      | Some default ->
          let base = Filename.basename default in
          let base =
            if
              base = Filename.current_dir_name ||
              System.is_root_directory base
            then
              None
            else
              Some base
          in
          let dir =
            let dir = Filename.dirname default in
            if dir = Filename.current_dir_name then
              System.get_cwd ()
            else
              System.make_filename_absolute dir
          in
          dir, base
  in
  let starting_directory = append_dir_sep_if_needed starting_directory in

  let rec browse ?default directory =
    let names =
      let append_dir_sep_if_directory filename =
        if System.is_directory (Filename.concat directory filename) then
          filename ^ Filename.dir_sep
        else
          filename
      in
      (* TODO: sort to show directories separately, and display them in another color *)
      append_dir_sep_if_needed Filename.parent_dir_name :: (
        System.ls directory
        |> sort_names
        |> List.map append_dir_sep_if_directory
      )
    in
    let choice =
      match default with
        | None ->
            (* The filter starts empty. Let's select the parent directory by default. *)
            Some 0
        | Some _ ->
            None
    in
    choose_from_list ?default ?choice (prompt ^ directory) names state @@ fun choice ->
    if choice = "" then
      ()
    else if choice = Filename.current_dir_name then
      browse directory
    else if choice = Filename.parent_dir_name || choice = append_dir_sep_if_needed Filename.parent_dir_name then
      browse (append_dir_sep_if_needed (Filename.dirname directory))
    else
      let filename = Filename.concat directory choice in
      if System.is_directory filename then
        browse filename
      else
        validate filename
  in
  browse ?default (append_dir_sep_if_needed starting_directory)

let display_help (state: State.t) make_page =
  let text, style, links = make_page state in

  (* Create help panel. *)
  let help_panel =
    let file = File.create ~read_only: true "help" text in
    let initial_layout = state.layout in
    let initial_focus = state.focus in
    let restore () =
      State.set_layout state initial_layout;
      State.set_focus state initial_focus;
    in
    let view = File.create_view (Help { restore; links }) file in
    view.style <- style;
    Panel.create view
  in

  (* Replace layout. *)
  State.set_layout state (Layout.single help_panel);
  State.set_focus state help_panel

(******************************************************************************)
(*                                 Definitions                                *)
(******************************************************************************)

let help { H.add } =
  add "Do nothing."

let () = define "noop" ~help Command @@ fun state ->
  ()

let help { H.line; par } =
  line "Exit the editor.";
  par ();
  line "Prompt for confirmation if there are modified files."

let () = define "quit" ~help Command @@ fun state ->
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

let help { H.line; par } =
  line "Open help.";
  par ();
  line "This opens the list of key bindings, and you can navigate to other help pages";
  line "from here.";
  par ();
  line "Press Q to go back to what you were doing."

let () = define "help" ~help Command @@ fun state ->
  display_help state Help.bindings_page

let () = define "help" ("page" -: String @-> Command) @@ fun page state ->
  display_help state (Help.page page)

let help { H.line } =
  line "Follow a link to another help page to get more information."

let () = define "follow_link" ~help Command @@ fun state ->
  let view = state.focus.view in
  match view.kind with
    | Help { links } ->
        File.if_only_one_cursor view @@ fun cursor ->
        (
          match Text.get cursor.position.x cursor.position.y links with
            | None | Some None ->
                ()
            | Some (Some link) ->
                display_help state (Help.page link)
        )
    | _ ->
        ()

let help { H.line } =
  line "Exit a prompt to go back to what you were doing."

let () = define "cancel" ~help Command @@ fun state ->
  match state.focus.view.kind with
    | Help { restore } ->
        (* TODO: if initial layout contains panels which view files which were killed, they need to change view. *)
        restore ()
    | Prompt _ ->
        State.remove_panel state.focus state
    | List_choice { original_view } ->
        (* TODO: if initial layout contains panels which view files which were killed, they need to change view. *)
        state.focus.view <- original_view
    | _ ->
        ()

let help { H.line; par; see_also } =
  line "Save file.";
  par ();
  line "If current file has no name, prompt for a filename.";
  see_also [ "save_as" ]

let () = define "save" ~help Command @@ fun state ->
  let file_to_save = state.focus.view.file in
  match file_to_save.filename with
    | None ->
        choose_from_file_system "Save as: " state (save file_to_save)
    | Some filename ->
        save file_to_save filename

let help { H.line; par; see_also } =
  line "Save file.";
  par ();
  line "Prompt for a filename even if current file already has one.";
  line "Existing file is not deleted.";
  see_also [ "save" ]

let () = define "save_as" ~help Command @@ fun state ->
  let file_to_save = state.focus.view.file in
  choose_from_file_system ?default: file_to_save.filename "Save as: " state (save file_to_save)

let help { H.line; par; see_also } =
  line "Open a file in the current panel.";
  par ();
  line "Prompt for the filename to open.";
  see_also [ "new"; "switch_file" ]

let () = define "open" ~help Command @@ fun state ->
  let panel = state.focus in
  choose_from_file_system "Open: " state @@ fun filename ->
  if not (System.file_exists filename) then abort "file does not exist: %S" filename;
  let file = State.create_file_loading state filename in
  let view = File.create_view File file in
  panel.view <- view

let help { H.line; par; see_also } =
  line "Create a new empty file.";
  par ();
  line "The file is not actually created on disk until you save.";
  see_also [ "open"; "switch_file" ]

let () = define "new" ~help Command @@ fun state ->
  let panel = state.focus in
  let file = State.create_file state "(new file)" Text.empty in
  let view = File.create_view File file in
  panel.view <- view

let help { H.line } =
  line "Remove current panel from current layout."

let () = define "remove_panel" ~help Command @@ fun state ->
  State.remove_panel state.focus state

let help { H.line; par; see_also } =
  line "Move cursor to the right.";
  par ();
  line "All cursors move one character to the right.";
  line "If there is no character to the right in the current line,";
  line "move to the beginning of the next line instead;";
  line "if there is no next line, don't move at all.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "move_left"; "move_down"; "move_up";
    "select_right"; "move_end_of_line"; "move_right_word";
  ]

let () = define "move_right" ~help Command @@ move true false move_right

let help { H.line; par; see_also } =
  line "Move cursor to the left.";
  par ();
  line "All cursors move one character to the left.";
  line "If there is no character to the left in the current line,";
  line "move to the end of the previous line instead;";
  line "if there is no previous line, don't move at all.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "move_right"; "move_down"; "move_up";
    "select_left"; "move_beginning_of_line"; "move_left_word";
  ]

let () = define "move_left" ~help Command @@ move true false move_left

let help { H.line; par; see_also } =
  line "Move cursor to the next line.";
  par ();
  line "All cursors move one line down.";
  line "If there is no line below do nothing.";
  line "Cursors move to their preferred column, or to the last character of the line";
  line "if preferred column is after the end of the line.";
  par ();
  line "Reset selection.";
  see_also [
    "move_up"; "move_right"; "move_left";
    "select_down"; "move_end_of_file"; "move_down_paragraph";
  ]

let () = define "move_down" ~help Command @@ move true true move_down

let help { H.line; par; see_also } =
  line "Move cursor to the previous line.";
  par ();
  line "All cursors move one line up.";
  line "If there is no line above do nothing.";
  line "Cursors move to their preferred column, or to the last character of the line";
  line "if preferred column is after the end of the line.";
  par ();
  line "Reset selection.";
  see_also [
    "move_down"; "move_right"; "move_left";
    "select_down"; "move_beginning_of_file"; "move_up_paragraph";
  ]

let () = define "move_up" ~help Command @@ move true true move_up

let help { H.line; par; see_also } =
  line "Move cursor to the end of the current line.";
  par ();
  line "All cursors move just after the character at the end of the current line.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "move_right"; "select_end_of_line";
    "move_beginning_of_line"; "move_end_of_file";
  ]

let () = define "move_end_of_line" ~help Command @@ move true false move_end_of_line

let help { H.line; par; see_also } =
  line "Move cursor to the beginning of the current line.";
  par ();
  line "All cursors move to the first character of the current line.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "move_left"; "select_beginning_of_line";
    "move_end_of_line"; "move_beginning_of_file";
  ]

let () = define "move_beginning_of_line" ~help Command @@ move true false move_beginning_of_line

let help { H.line; par; see_also } =
  line "Move cursor to the end of the current file.";
  par ();
  line "All cursors move just after the last character of the last line.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "select_end_of_file"; "move_end_of_line"; "move_beginning_of_file";
  ]

let () = define "move_end_of_file" ~help Command @@ move true false move_end_of_file

let help { H.line; par; see_also } =
  line "Move cursor to the beginning of the current file.";
  par ();
  line "All cursors move to the first character of the first line.";
  par ();
  line "Reset selection and preferred column.";
  see_also [
    "select_beginning_of_file"; "move_beginning_of_line"; "move_end_of_file";
  ]

let () = define "move_beginning_of_file" ~help Command @@ move true false move_beginning_of_file

let help { H.line; par; see_also } =
  line "Move cursor to the end of the word.";
  par ();
  line "All cursors move to the first character after the current word,";
  line "or of the next word if they are not in a word.";
  par ();
  line "A word is a sequence of letters or digits.";
  par ();
  line "Reset selection and preferred column.";
  see_also [ "select_right_word"; "move_left_word" ]

let () = define "move_right_word" ~help Command @@ move true false move_right_word

let help { H.line; par; see_also } =
  line "Move cursor to the beginning of the word.";
  par ();
  line "All cursors move to the first character of the current word,";
  line "or of the previous word if they are not in a word.";
  par ();
  line "A word is a sequence of letters or digits.";
  par ();
  line "Reset selection and preferred column.";
  see_also [ "select_left_word"; "move_right_word" ]

let () = define "move_left_word" ~help Command @@ move true false move_left_word

let help { H.line; par; see_also } =
  line "Move cursor to the end of the paragraph.";
  par ();
  line "All cursors move to the first line after the current paragraph,";
  line "or of the next paragraph if they are not in a paragraph.";
  par ();
  line "A paragraph is a sequence of non-empty lines.";
  par ();
  line "Reset selection.";
  see_also [ "select_down_paragraph"; "move_up_paragraph" ]

let () = define "move_down_paragraph" ~help Command @@ move true false move_down_paragraph

let help { H.line; par; see_also } =
  line "Move cursor to the beginning of the paragraph.";
  par ();
  line "All cursors move to the first line before the current paragraph,";
  line "or of the previous paragraph if they are not in a paragraph.";
  par ();
  line "A paragraph is a sequence of non-empty lines.";
  par ();
  line "Reset selection.";
  see_also [ "select_up_paragraph"; "move_down_paragraph" ]

let () = define "move_up_paragraph" ~help Command @@ move true false move_up_paragraph

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_right"; add " but does not reset selection."; nl ()

let () = define "select_right" ~help Command @@ move false false move_right

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_left"; add " but does not reset selection."; nl ()

let () = define "select_left" ~help Command @@ move false false move_left

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_down"; add " but does not reset selection."; nl ()

let () = define "select_down" ~help Command @@ move false true move_down

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_up"; add " but does not reset selection."; nl ()

let () = define "select_up" ~help Command @@ move false true move_up

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_end_of_line"; add " but does not reset selection."; nl ()

let () = define "select_end_of_line" ~help Command @@ move false false move_end_of_line

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_beginning_of_line"; add " but does not reset selection."; nl ()

let () = define "select_beginning_of_line" ~help Command @@ move false false move_beginning_of_line

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_end_of_file"; add " but does not reset selection."; nl ()

let () = define "select_end_of_file" ~help Command @@ move false false move_end_of_file

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_beginning_of_file"; add " but does not reset selection."; nl ()

let () = define "select_beginning_of_file" ~help Command @@ move false false move_beginning_of_file

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_right_word"; add " but does not reset selection."; nl ()

let () = define "select_right_word" ~help Command @@ move false false move_right_word

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_left_word"; add " but does not reset selection."; nl ()

let () = define "select_left_word" ~help Command @@ move false false move_left_word

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_down_paragraph"; add " but does not reset selection."; nl ()

let () = define "select_down_paragraph" ~help Command @@ move false false move_down_paragraph

let help { H.add; nl; add_link } =
  add "Same as "; add_link "move_up_paragraph"; add " but does not reset selection."; nl ()

let () = define "select_up_paragraph" ~help Command @@ move false false move_up_paragraph

let help { H.line; par; see_also } =
  line "Select all text.";
  par ();
  line "Set selection of all cursors to start at the beginning of the file.";
  line "Set position of all cursors to the end of the file.";
  line "Reset preferred column.";
  see_also [
    "select_right";
    "select_end_of_line";
    "select_end_of_file";
    "select_right_word";
    "select_down_paragraph";
  ]

let () = define "select_all" ~help Command @@ fun state ->
  select_all state.focus.view

let help { H.line; par; see_also } =
  line "Give focus to the panel at the right of the current one.";
  par ();
  line "Commands which act on a view act on the view of the focused panel.";
  line "Only one panel has focus at a given time.";
  see_also [ "focus_left"; "focus_down"; "focus_up" ]

let () = define "focus_right" ~help Command @@ focus_relative Layout.get_panel_right

let help { H.line; par; see_also } =
  line "Give focus to the panel at the left of the current one.";
  par ();
  line "Commands which act on a view act on the view of the focused panel.";
  line "Only one panel has focus at a given time.";
  see_also [ "focus_right"; "focus_down"; "focus_up" ]

let () = define "focus_left" ~help Command @@ focus_relative Layout.get_panel_left

let help { H.line; par; see_also } =
  line "Give focus to the panel below the current one.";
  par ();
  line "Commands which act on a view act on the view of the focused panel.";
  line "Only one panel has focus at a given time.";
  see_also [ "focus_right"; "focus_left"; "focus_up" ]

let () = define "focus_down" ~help Command @@ focus_relative Layout.get_panel_down

let help { H.line; par; see_also } =
  line "Give focus to the panel above the current one.";
  par ();
  line "Commands which act on a view act on the view of the focused panel.";
  line "Only one panel has focus at a given time.";
  see_also [ "focus_right"; "focus_left"; "focus_down" ]

let () = define "focus_up" ~help Command @@ focus_relative Layout.get_panel_up

let help { H.line; par; see_also } =
  line "Scroll half a page down.";
  par ();
  line "If there is only one cursor, also move it half a page down.";
  line "It moves to its preferred column, or to the last character of the line";
  line "if preferred column is after the end of the line.";
  see_also [ "scroll_up" ]

let () = define "scroll_down" ~help Command @@ fun state ->
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

let help { H.line; par; see_also } =
  line "Scroll half a page up.";
  par ();
  line "If there is only one cursor, also move it half a page up.";
  line "It moves to its preferred column, or to the last character of the line";
  line "if preferred column is after the end of the line.";
  see_also [ "scroll_down" ]

let () = define "scroll_up" ~help Command @@ fun state ->
  let view = state.focus.view in

  (* Scroll. *)
  let old_scroll = view.scroll_y in
  view.scroll_y <- max 0 (view.scroll_y - view.height / 2);

  (* Move cursor. *)
  move_after_scroll view old_scroll

let help { H.line; par; see_also } =
  line "Split line at cursor.";
  par ();
  line "The end of the line moves to a new line below.";
  line "Cursor moves to the beginning of this new line."

let () = define "insert_new_line" ~help Command @@ fun state ->
  let view = state.focus.view in
  File.replace_selection_by_new_line view;
  File.recenter_if_needed view

let help { H.line; par; see_also } =
  line "Delete the next character.";
  par ();
  line "If cursor is at the end of a line, merge the next line instead.";
  line "If selection is not empty, delete it instead.";
  line "Reset preferred column.";
  see_also [
    "delete_character_backwards";
    "delete_end_of_line";
    "delete_end_of_word";
  ]

let () = define "delete_character" ~help Command @@ fun state ->
  let view = state.focus.view in
  File.delete_selection_or_character view;
  File.recenter_if_needed view

let help { H.line; par; see_also } =
  line "Delete the previous character.";
  par ();
  line "If cursor is at the beginning of a line, merge this line";
  line "into the previous line instead.";
  line "If selection is not empty, delete it instead.";
  line "Reset preferred column.";
  see_also [
    "delete_character";
    "delete_beginning_of_word";
  ]

let () = define "delete_character_backwards" ~help Command @@ fun state ->
  let view = state.focus.view in
  File.delete_selection_or_character_backwards view;
  File.recenter_if_needed view

let help { H.line; par; see_also } =
  line "Delete the end of the current line.";
  par ();
  line "If cursor is at the end of a line, merge the next line instead.";
  see_also [
    "delete_character";
    "delete_end_of_word";
  ]

let () = define "delete_end_of_line" ~help Command @@ fun state ->
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

let help { H.line; par; see_also } =
  line "Delete the end of the current word.";
  par ();
  line "If cursor is not in a word, delete until the end of the next word.";
  see_also [
    "move_right_word";
    "delete_character";
    "delete_beginning_of_word";
  ]

let () = define "delete_end_of_word" ~help Command @@ fun state ->
  let view = state.focus.view in
  (
    File.delete_from_cursors view @@ fun text cursor ->
    move_right_word text cursor.position.x cursor.position.y
  );
  File.recenter_if_needed view

let help { H.line; par; see_also } =
  line "Delete the beginning of the current word.";
  par ();
  line "If cursor is not in a word, delete until the beginning of the previous word.";
  see_also [
    "move_left_word";
    "delete_character_backwards";
    "delete_end_of_word";
  ]

let () = define "delete_beginning_of_word" ~help Command @@ fun state ->
  let view = state.focus.view in
  (
    File.delete_from_cursors view @@ fun text cursor ->
    move_left_word text cursor.position.x cursor.position.y
  );
  File.recenter_if_needed view

let help { H.add; line; nl; par; add_link } =
  line "Create one cursor per selected line.";
  par ();
  line "If there are more than one cursor already, remove all cursors but one instead.";
  par ();
  add "Commands which apply to cursors, such as "; add_link "move_right_word";
  add " or "; add_link "delete_word"; add ","; nl ();
  line "are applied to all cursors.";
  par ();
  add "Clipboard commands, such as "; add_link "copy"; add " and "; add_link "paste";
  add ", use the cursor clipboard instead"; nl ();
  line "of the global clipboard. This means that you can have each cursor copy its";
  line "own selection, and paste it somewhere else.";
  par ();
  add "Other commands, such as "; add_link "save_as"; add ", are only run once."; nl ()

let () = define "create_cursors_from_selection" ~help Command @@ fun state ->
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

let help { H.add; line; nl; par; add_link; see_also } =
  line "Copy selection to clipboard.";
  par ();
  line "If there is only one cursor, selection is copied to the global clipboard.";
  line "It can be pasted from any view, in any file.";
  par ();
  add "If there are several cursor (see "; add_link "create_cursors_from_selection";
  add "),"; nl ();
  line "the selection of each cursor is copied to the local clipboard of each cursor.";
  see_also [ "cut"; "paste" ]

let () = define "copy" ~help Command @@ fun state -> File.copy state.clipboard state.focus.view

let help { H.add; line; nl; par; add_link; see_also } =
  line "Copy selection to clipboard, then delete selection.";
  see_also [ "copy"; "paste" ]

let () = define "cut" ~help Command @@ fun state ->
  let view = state.focus.view in
  File.cut state.clipboard view;
  File.recenter_if_needed view

let help { H.add; line; nl; par; add_link; see_also } =
  line "Paste from clipboard.";
  par ();
  line "Selection is deleted before pasting.";
  par ();
  line "If there is only one cursor, paste selection from the global clipboard.";
  add "If there are several cursor (see "; add_link "create_cursors_from_selection";
  add "),"; nl ();
  line "for each cursor, paste the clipboard of this cursor at its position.";
  see_also [ "cut"; "paste" ]

let () = define "paste" ~help Command @@ fun state ->
  let view = state.focus.view in
  File.paste state.clipboard view;
  File.recenter_if_needed view

let help { H.add; line; nl; par; add_link; see_also } =
  line "Undo recent edits.";
  par ();
  line "You can undo repeatedly until the point where the current file was last saved.";
  see_also [ "redo" ]

let () = define "undo" ~help Command @@ fun state -> File.undo state.focus.view.file

let help { H.add; line; nl; par; add_link; see_also } =
  line "Redo what was recently undone.";
  par ();
  line "You can redo repeatedly until you come back to the point of the first undo";
  line "of the last undo sequence.";
  par ();
  line "Any edit which is not a redo or an undo will remove the possibility to redo.";
  see_also [ "undo" ]

let () = define "redo" ~help Command @@ fun state -> File.redo state.focus.view.file

let help { H.add; line; nl; par; add_link; see_also } =
  line "Validate selected choice.";
  par ();
  add "In a prompt, such as the one which appears when you "; add_link "quit"; add ","; nl ();
  line "validate the text you typed.";
  par ();
  add "In a list of choices, such as the one which appears when you "; add_link "save_as"; add ","; nl ();
  line "validate selected choice. If you did not select anything,";
  line "validate the text you typed instead."

let () = define "validate" ~help Command @@ fun state ->
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
            | exception (Invalid_argument _ | Failure _) ->
                validate filter
            | choice ->
                validate choice
        )
    | _ ->
        abort "focused panel is not a prompt"

let help { H.add; line; nl; par; add_link; see_also } =
  line "Execute a command.";
  par ();
  add "Prompt for a command name, such as "; add_link "save_as"; add " or "; add_link "move_left"; add ","; nl ();
  line "and execute this command.";
  see_also [ "execute_process" ]

let () = define "execute_command" ~help Command @@ fun state ->
  prompt "Execute command: " state @@ fun command ->
  state.run_string command state

let help { H.add; line; nl; par; add_link; see_also } =
  line "Execute an external command.";
  par ();
  add "Prompt for a program name and its arguments, such as ";
  add ~style: (Style.bold ()) "ls -la"; add ", and execute it."; nl();
  par ();
  line "Run the process in the background. Display program output in the current panel.";
  see_also [ "execute_command" ]

let () = define "execute_process" ~help Command @@ fun state ->
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

let help { H.line; par; see_also } =
  line "Switch to another already-opened file.";
  par ();
  line "Prompt for the filename to switch to.";
  see_also [ "new"; "open" ]

let () = define "switch_file" ~help Command @@ fun state ->
  let panel = state.focus in
  let names = sort_names (List.map File.get_name state.files) in
  choose_from_list ~choice: 0 "Switch to file: " names state @@ fun choice ->
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

let help { H.line; add; nl; add_link; par; see_also } =
  line "Select the item above the currently selected one.";
  par ();
  add "If you "; add_link "validate"; add " and an item is selected, choose this item"; nl ();
  line "instead of what you typed.";
  par ();
  line "If no item is selected, select the first one, i.e. the one at the bottom.";
  see_also [ "choose_previous" ]

let () = define "choose_next" ~help Command @@ fun state ->
  match state.focus.view.kind with
    | List_choice choice ->
        let choices = Panel.filter_choices (Text.to_string state.focus.view.file.text) choice.choices in
        choice.choice <- choice.choice + 1;
        let max_choice = List.length choices - 1 in
        if choice.choice > max_choice then choice.choice <- max_choice
    | _ ->
        abort "focused panel is not a prompt"

let help { H.line; add; nl; add_link; par; see_also } =
  line "Select the item below the currently selected one.";
  par ();
  add "If you "; add_link "validate"; add " and an item is selected, choose this item"; nl ();
  line "instead of what you typed.";
  par ();
  line "If the first item is selected, i.e. the one at the bottom, unselect it instead.";
  see_also [ "choose_next" ]

let () = define "choose_previous" ~help Command @@ fun state ->
  match state.focus.view.kind with
    | List_choice choice ->
        choice.choice <- choice.choice - 1;
        if choice.choice < -1 then choice.choice <- -1
    | _ ->
        abort "focused panel is not a prompt"
