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

let on_key_press state (key: Key.t) =
  match Key.Map.find key state.bindings with
    | exception Not_found ->
        Log.infof "unbound key: %s" (Key.show key)
    | command ->
        command state

let render state frame =
  Layout.render
    (Panel.render state.focus)
    frame
    (Render.width frame)
    (Render.height frame)
    state.layout
