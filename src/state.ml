type t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;

    (* Global bindings can be overridden by local bindings. *)
    mutable global_bindings: (t -> unit) Key.Map.t;
    mutable file_bindings: (t -> unit) Key.Map.t;
    mutable prompt_bindings: (t -> unit) Key.Map.t;

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
                    File.replace_selection_by_character (String.make 1 char) state.focus.view
                | Unicode character ->
                    File.replace_selection_by_character character state.focus.view
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
