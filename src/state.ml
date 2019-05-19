type t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;

    (* Global bindings can be overridden by local bindings. *)
    mutable global_bindings: (t -> unit) Key.Map.t;
    mutable file_bindings: (t -> unit) Key.Map.t;
    mutable prompt_bindings: (t -> unit) Key.Map.t;

    (* Files to check for modification before exiting.
       Also files that should not be reopened (reuse them instead). *)
    mutable files: File.t list;

    clipboard: Clipboard.t;
  }

let create ?focus layout =
  let focus =
    match focus with
      | None ->
          Layout.get_main_panel layout
      | Some focus ->
          focus
  in
  {
    layout;
    focus;
    global_bindings = Key.Map.empty;
    file_bindings = Key.Map.empty;
    prompt_bindings = Key.Map.empty;
    clipboard = { text = Text.empty };
    files = [];
  }

exception Exit
exception Abort

let on_key_press state (key: Key.t) =
  let local_bindings =
    match state.focus.kind with
      | File -> state.file_bindings
      | Prompt _ -> state.prompt_bindings
  in
  let catch f x =
    try
      f x
    with
      | Abort ->
          ()
      | System.Error reason ->
          Log.error "%s" reason
  in
  match Key.Map.find key local_bindings with
    | command ->
        catch command state
    | exception Not_found ->
        match Key.Map.find key state.global_bindings with
          | command ->
              catch command state
          | exception Not_found ->
              match Key.symbol key with
                | ASCII char ->
                    catch (File.replace_selection_by_character (String.make 1 char)) state.focus.view
                | Unicode character ->
                    catch (File.replace_selection_by_character character) state.focus.view
                | Control ->
                    Log.info "unbound key: %s" (Key.show key)

let render state frame =
  Layout.render
    (Panel.render state.focus)
    frame
    (Render.width frame)
    (Render.height frame)
    state.layout

let set_layout state layout =
  state.layout <- layout

let create_file state text =
  let file = File.create text in
  state.files <- file :: state.files;
  file

let create_file_loading state filename =
  let file = create_file state Text.empty in
  File.load file filename;
  file
