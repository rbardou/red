type t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;
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
  }

exception Exit

let on_key_press state (key: Key.t) =
  if key = Ctrl_c then raise Exit;
  () (* TODO *)

let render state frame =
  Layout.render
    (Panel.render state.focus)
    frame
    (Render.width frame)
    (Render.height frame)
    state.layout
