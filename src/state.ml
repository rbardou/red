open Misc

module Context =
struct
  type t =
    | Global
    | File
    | Prompt
    | Search
    | List_choice
    | Help

  let compare = Pervasives.compare
end

module Context_map = Map.Make (Context)

module History_context_map = Map.Make (File.History_context)

module History =
struct
  type t = string list

  let empty = []

  let max_count = 100

  let add item (history: t) =
    let history = item :: List.filter ((<>) item) history in
    truncate_list max_count history

  let to_list (history: t) =
    history
end

type t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;
    clipboard: Clipboard.t;

    (* Global bindings can be overridden by local bindings. *)
    mutable bindings: (t -> unit) Key.Map.t Context_map.t;

    (* Files to check for modification before exiting.
       Also files that should not be reopened (reuse them instead). *)
    mutable files: File.t list;

    (* The two following functions are Redl.run_file and Redl.run_string, but with [env] hidden in their closure
       (because otherwise State would depend on Redl which already depends on State).

       After the first argument is applied, the program is parsed and typed.
       This means that the environment which is used is the one available when applying the fisrt argument.
       But the command is not actually run until the state argument is given. *)
    run_file: string -> t -> unit;
    run_string: string -> t -> unit;

    mutable history: History.t History_context_map.t;
    mutable last_message: string;
  }

let create ?focus ~run_file ~run_string layout =
  let focus =
    match focus with
      | None ->
          Layout.get_main_panel layout
      | Some focus ->
          focus
  in
  let state =
    {
      layout;
      focus;
      clipboard = { text = Text.empty };
      bindings = Context_map.empty;
      files = [];
      run_file;
      run_string;
      history = History_context_map.empty;
      last_message = "";
    }
  in
  let on_log message = state.last_message <- message in
  Log.add_handler on_log;
  state

exception Exit
exception Abort

let abort ?exn reason =
  Log.error ?exn "%s" reason;
  raise Abort

let abort ?exn x = Printf.ksprintf (abort ?exn) x

let get_focused_main_view state =
  Panel.get_current_main_view state.focus

let get_focused_view state =
  Panel.get_current_view state.focus

let set_focused_view state view =
  Panel.set_current_view state.focus view

let get_focused_file state =
  (get_focused_view state).file

let get_context_bindings context state =
  match Context_map.find context state.bindings with
    | exception Not_found ->
        Key.Map.empty
    | bindings ->
        bindings

let get_context state: Context.t =
  match (get_focused_view state).kind with
    | Prompt -> Prompt
    | Search _ -> Search
    | File -> File
    | List_choice _ -> List_choice
    | Help _ -> Help

let get_local_bindings state =
  get_context_bindings (get_context state) state

let get_global_bindings state =
  get_context_bindings Global state

let on_key_press state (key: Key.t) =
  state.last_message <- "";
  let catch f x =
    try
      f x
    with
      | Abort ->
          ()
      | System.Error reason ->
          Log.error "%s" reason
  in
  match Key.Map.find key (get_local_bindings state) with
    | run ->
        catch run state
    | exception Not_found ->
        match Key.Map.find key (get_global_bindings state) with
          | run ->
              catch run state
          | exception Not_found ->
              match Key.symbol key with
                | ASCII char ->
                    catch (File.replace_selection_by_character (String.make 1 char)) (get_focused_view state)
                | Unicode character ->
                    catch (File.replace_selection_by_character character) (get_focused_view state)
                | Control ->
                    Log.info "unbound key: %s" (Key.show key)

let render state frame =
  let w = Render.width frame in
  let h = Render.height frame in
  Layout.render
    (Panel.render state.focus)
    frame
    w
    (h - 1)
    state.layout;
  Render.text frame 0 (h - 1) w state.last_message

let set_layout state layout =
  state.layout <- layout

let create_file state name text =
  let file = File.create name text in
  state.files <- file :: state.files;
  file

let create_file_loading state filename =
  let file = create_file state filename Text.empty in
  File.load file filename;
  file

let remove_panel panel state =
  match Layout.remove_panel panel state.layout with
    | None ->
        abort "cannot remove last panel"
    | Some (new_layout, next_panel) ->
        set_layout state new_layout;
        state.focus <- next_panel

let set_focus state focus =
  state.focus <- focus

let create_empty_file (state: t) =
  create_file state "(new file)" Text.empty

let get_default_view (state: t) =
  match state.files with
    | [] ->
        let file = create_empty_file state in
        File.create_view File file
    | file :: _ ->
        match file.views with
          | [] ->
              File.create_view File file
          | view :: _ ->
              File.copy_view view

let close_file file state =
  state.files <- List.filter ((!=) file) state.files;
  Layout.foreach_panel state.layout (Panel.remove_file file (fun () -> get_default_view state))

let add_history context item state =
  let previous_history =
    match History_context_map.find context state.history with
      | exception Not_found ->
          History.empty
      | history ->
          history
  in
  let new_history = History.add item previous_history in
  state.history <- History_context_map.add context new_history state.history

let get_history context state =
  let history =
    match History_context_map.find context state.history with
      | exception Not_found ->
          History.empty
      | history ->
          history
  in
  History.to_list history
