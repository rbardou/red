type t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;
    mutable bindings: (t -> unit) Key.Map.t;
  }

let create ?focus layout =
  let focus =
    match focus with
      | None ->
          Layout.get_first_panel layout
      | Some focus ->
          focus
  in
  {
    layout;
    focus;
    bindings = Key.Map.empty;
  }

exception Exit

let replace_selection_by_character character state =
  (* TODO: group this under a "modify" wrapper, which would reset preferred x automatically,
     remove the need for if_writable, and possibly add to undo list *)
  File.delete_selection state.focus.view;
  File.insert_character character state.focus.view;
  File.reset_preferred_x state.focus.view.file

let replace_selection_by_new_line state =
  (* TODO: group this under a "modify" wrapper, which would reset preferred x automatically,
     remove the need for if_writable, and possibly add to undo list *)
  File.delete_selection state.focus.view;
  File.insert_new_line state.focus.view;
  File.reset_preferred_x state.focus.view.file

let on_key_press state (key: Key.t) =
  match Key.Map.find key state.bindings with
    | command ->
        command state
    | exception Not_found ->
        match Key.symbol key with
          | ASCII char ->
              replace_selection_by_character (String.make 1 char) state
          | Unicode character ->
              replace_selection_by_character character state
          | Control ->
              Log.infof "unbound key: %s" (Key.show key)

let render state frame =
  Layout.render
    (Panel.render state.focus)
    frame
    (Render.width frame)
    (Render.height frame)
    state.layout
