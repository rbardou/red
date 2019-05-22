type prompt =
  {
    prompt: string;
    validate: Text.t -> unit;
  }

type kind =
  | File
  | Prompt of prompt

type t =
  {
    kind: kind;
    mutable view: File.view;
  }

let create kind view =
  {
    kind;
    view;
  }

let create_file file =
  {
    kind = File;
    view = File.create_view file;
  }

(* Render text and cursors for a given view. *)
let render_view
    ~style
    ~cursor_style
    ~selection_style
    (frame: Render.frame) (view: File.view) ~x ~y ~w ~h =
  view.width <- w;
  view.height <- h;
  let file = view.file in
  let text = file.text in
  let scroll_x = view.scroll_x in
  let scroll_y =
    if view.auto_scroll_to_bottom then
      (* Get last line index (minus one if empty). *)
      let last_line = Text.get_line_count text in
      let last_line =
        if Text.get_line_length last_line text = 0 then
          max 0 (last_line - 1)
        else
          last_line
      in

      (* Unless everything already fits, scroll. *)
      if last_line + 1 > h then
        last_line - h
      else
        0
    else
      view.scroll_y
  in
  let cursors = view.cursors in

  (* Return true whether a given position is the position of a cursor. *)
  let cursor_is_at x y (cursor: File.cursor) =
    cursor.position.x = x && cursor.position.y = y
  in

  (* Text area. *)
  for text_y = scroll_y to scroll_y + h - 1 do
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
          cursor_style
        else if List.exists (File.cursor_is_in_selection text_x text_y) cursors then
          selection_style
        else
          style
      in
      Render.set frame (x + text_x - scroll_x) (y + text_y - scroll_y) (Render.cell ~style character)
    done;
  done

(* Render status bar for a panel of kind [File]. *)
let render_file_status_bar has_focus (frame: Render.frame) (view: File.view) ~x ~y ~w =
  let file = view.file in
  let style =
    if has_focus then
      Render.style ~bg_color: Cyan ~fg_color: Black ()
    else
      Render.style ~bg_color: White ~fg_color: Black ()
  in

  (* Render "Modified" flag. *)
  Render.set frame x y (
    if file.modified then
      Render.cell ~style: { style with fg_color = Black; bg_color = Red } "*"
    else
      Render.cell ~style " "
  );

  (* Render status text. *)
  let status_text =
    let loading =
      match file.loading with
        | No ->
            ""
        | File { loaded; size } ->
            Printf.sprintf "(Loading: %d%%)" (loaded * 100 / size)
        | Process name ->
            "(Running...)"
    in
    let process_status =
      match file.process_status with
        | None | Some (WEXITED 0) ->
            ""
        | Some (WEXITED code) ->
            Printf.sprintf "(exited with code %d)" code
        | Some (WSIGNALED signal) ->
            Printf.sprintf "(killed by signal %d)" signal
        | Some (WSTOPPED signal) ->
            Printf.sprintf "(stopped by signal %d)" signal
    in
    let cursors =
      match view.cursors with
        | [ cursor ] ->
            Printf.sprintf "(%d, %d)" cursor.position.x cursor.position.y
        | cursors ->
            Printf.sprintf "(%d cursors)" (List.length cursors)
    in
    String.concat " " (List.filter ((<>) "") [ file.name; loading; process_status; cursors ])
  in
  Render.text ~style frame (x + 1) y (w - 1) status_text

let render focused_panel (frame: Render.frame) panel ~x ~y ~w ~h =
  let has_focus = panel == focused_panel in
  let view = panel.view in
  match panel.kind with
    | File ->
        let cursor_style =
          if has_focus then
            Render.style ~fg_color: Black ~bg_color: Cyan ()
          else
            Render.style ~fg_color: Black ~bg_color: Yellow ()
        in
        render_view
          ~style: Render.default
          ~cursor_style
          ~selection_style: (Render.style ~fg_color: Black ~bg_color: White ())
          frame view ~x ~y ~w ~h: (h - 1);
        render_file_status_bar has_focus frame view ~x ~y: (y + h - 1) ~w

    | Prompt { prompt } ->
        let prompt_length = String.length prompt in (* We assume unicode here for now. *)

        (* Render prompt. *)
        let style =
          if has_focus then
            Render.style ~bg_color: Cyan ~fg_color: Black ()
          else
            Render.style ~bg_color: White ~fg_color: Black ()
        in
        Render.text ~style frame x y (min w prompt_length) prompt;

        (* Render prompt input area. *)
        render_view
          ~style
          ~cursor_style: (Render.style ~fg_color: Black ~bg_color: Yellow ())
          ~selection_style: (Render.style ~fg_color: Black ~bg_color: White ())
          frame view ~x: (x + prompt_length) ~y ~w: (w - prompt_length) ~h
