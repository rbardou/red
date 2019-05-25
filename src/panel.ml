type t =
  {
    mutable view: File.view;
  }

let create view =
  {
    view;
  }

let create_file file =
  create (File.create_view File file)

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
  let view_style = view.style in
  let scroll_x = view.scroll_x in
  let scroll_y = view.scroll_y in
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
          match Text.get text_x text_y view_style with
            | None -> style
            | Some style -> style
      in
      Render.set frame (x + text_x - scroll_x) (y + text_y - scroll_y) (Render.cell ~style character)
    done;
  done

(* Render status bar for a panel of kind [File]. *)
let render_file_status_bar has_focus (frame: Render.frame) (view: File.view) ~x ~y ~w =
  let file = view.file in
  let style =
    if has_focus then
      Style.make ~bg: Cyan ~fg: Black ()
    else
      Style.make ~bg: White ~fg: Black ()
  in

  (* Render "Modified" flag. *)
  Render.set frame x y (
    if file.modified then
      Render.cell ~style: { style with fg = Black; bg = Red } "*"
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
            if size = 0 then
              "(Loading: 100%)"
            else
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

let render_prompt has_focus (frame: Render.frame) view prompt ~x ~y ~w =
  (* For now we assume prompts contain only ASCII characters. *)
  let prompt_length = String.length prompt in

  (* Render prompt. *)
  let style =
    if has_focus then
      Style.make ~bg: Cyan ~fg: Black ()
    else
      Style.make ~bg: White ~fg: Black ()
  in
  Render.text ~style frame x y (min w prompt_length) prompt;

  (* Render prompt input area. *)
  render_view
    ~style
    ~cursor_style: (Style.make ~fg: Black ~bg: Yellow ())
    ~selection_style: (Style.make ~fg: Black ~bg: White ())
    frame view ~x: (x + prompt_length) ~y ~w: (w - prompt_length) ~h: 1

let render_choice_list (frame: Render.frame) choices choice ~x ~y ~w ~h =
  let rec loop index choices =
    let y = y + h - 1 - index in
    if y >= 0 then
      match choices with
        | [] ->
            ()
        | head :: tail ->
            let style =
              if index = choice then
                Style.make ~fg: Black ~bg: White ()
              else
                Style.default
            in
            Render.text ~style frame x y w head;
            loop (index + 1) tail
  in
  loop 0 choices

let filter_choices filter choices =
  let filters = Filter_lexer.items [] (Lexing.from_string filter) in
  let matches choice filter =
    (* TODO: case-sensitive match if filter has capital letters *)
    let choice = String.lowercase_ascii choice in
    let filter = String.lowercase_ascii filter in
    let choice_length = String.length choice in
    let filter_length = String.length filter in
    let rec loop start =
      if start + filter_length > choice_length then
        false
      else if String.sub choice start filter_length = filter then
        true
      else
        loop (start + 1)
    in
    loop 0
  in
  List.filter (fun choice -> List.for_all (matches choice) filters) choices

let render focused_panel (frame: Render.frame) panel ~x ~y ~w ~h =
  let has_focus = panel == focused_panel in
  let view = panel.view in
  match panel.view.kind with
    | File ->
        let cursor_style =
          if has_focus then
            Style.make ~fg: Black ~bg: Cyan ()
          else
            Style.make ~fg: Black ~bg: Yellow ()
        in
        render_view
          ~style: Style.default
          ~cursor_style
          ~selection_style: (Style.make ~fg: Black ~bg: White ())
          frame view ~x ~y ~w ~h: (h - 1);
        render_file_status_bar has_focus frame view ~x ~y: (y + h - 1) ~w

    | Prompt { prompt } ->
        render_prompt has_focus frame view prompt ~x ~y ~w

    | List_choice { prompt; choices; choice } ->
        let filter = Text.to_string view.file.text in
        let choices = filter_choices filter choices in
        render_choice_list frame choices choice ~x ~y ~w ~h: (h - 1);
        render_prompt has_focus frame view prompt ~x ~y: (y + h - 1) ~w
