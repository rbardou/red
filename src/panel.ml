type t =
  {
    mutable view: File.view;
  }

let create view =
  {
    view;
  }

let make_file_status (file: File.t) =
  let filename =
    match file.filename with
      | None ->
          "(unnamed file)"
      | Some filename ->
          filename
  in

  match file.loading with
    | None ->
        filename
    | Some (loaded, size, _) ->
        Printf.sprintf "%s (Loading: %d%%)" filename (loaded * 100 / size)

let status_bar_style_without_focus =
  Render.style ~bg_color: White ~fg_color: Black ()

let status_bar_style_with_focus =
  Render.style ~bg_color: Cyan ~fg_color: Black ()

let status_bar_style has_focus =
  if has_focus then
    status_bar_style_with_focus
  else
    status_bar_style_without_focus

let render focused_panel (frame: Render.frame) panel ~x ~y ~w ~h =
  let has_focus = panel == focused_panel in
  let scroll_x = panel.view.scroll_x in
  let scroll_y = panel.view.scroll_y in
  let cursors = panel.view.cursors in

  (* Return true whether a given position is the position of a cursor. *)
  let cursor_is_at x y (cursor: File.cursor) =
    cursor.position.x = x && cursor.position.y = y
  in

  (* Text area. *)
  let text = panel.view.file.text in
  for text_y = scroll_y to scroll_y + h - 2 do
    for text_x = scroll_x to scroll_x + w - 1 do
      let character =
        match Text.get text_x text_y text with
          | None ->
              " "
          | Some c ->
              c
      in
      let character =
        if character = "\t" then
          " "
        else if String.length character = 1 && character.[0] < '\032' then
          "?"
        else
          character
      in
      (* TODO: show trailing spaces *)
      let style =
        if List.exists (cursor_is_at text_x text_y) cursors then
          Render.style ~fg_color: Black ~bg_color: (if has_focus then Cyan else Yellow) ()
        else if List.exists (File.cursor_is_in_selection text_x text_y) cursors then
          Render.style ~fg_color: Black ~bg_color: White ()
        else
          Render.default
      in
      Render.set frame (x + text_x - scroll_x) (y + text_y - scroll_y) (Render.cell ~style character)
    done;
  done;

  (* Status bar. *)
  Render.text ~style: (status_bar_style has_focus) frame x (y + h - 1) w
    (make_file_status panel.view.file)
