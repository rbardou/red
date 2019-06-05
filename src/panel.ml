open Misc

type t =
  {
    mutable view: File.view; (* current view *)

    (* Contains only [File] views. *)
    mutable previous_views: File.view list;
  }

let get_current_main_view panel =
  panel.view

let get_current_view panel =
  let view = panel.view in
  match view.prompt with
    | Some { prompt_view } ->
        prompt_view
    | None ->
        match view.search with
          | None ->
              view
          | Some { search_view } ->
              search_view

let set_current_view panel (view: File.view) =
  (* If current view is a File view, add it to previous views. *)
  (
    match panel.view.kind with
      | File ->
          panel.previous_views <- panel.view :: panel.previous_views
      | Prompt | Search _ | List_choice _ | Help _ ->
          ()
  );

  (* Remove previous views which already visit the same file as [view].
     This may include the view that we just added. *)
  let not_same_file (previous_view: File.view) = previous_view.file != view.file in
  panel.previous_views <- List.filter not_same_file panel.previous_views;

  (* Set current view. *)
  panel.view <- view

let set_current_file panel file =
  match
    let same_file (view: File.view) = view.file == file in
    List.find same_file (panel.view :: panel.previous_views)
  with
    | view ->
        set_current_view panel view
    | exception Not_found ->
        match file.views with
          | [] ->
              set_current_view panel (File.create_view File file)
          | view :: _ ->
              set_current_view panel (File.copy_view view)

let kill_current_view panel =
  match panel.previous_views with
    | head :: tail ->
        panel.view <- head;
        panel.previous_views <- tail;
        true
    | [] ->
        false

let remove_file file get_default_view panel =
  panel.previous_views <- List.filter (fun (view: File.view) -> view.file != file) panel.previous_views;
  if panel.view.file == file then
    panel.view <- (get_default_view ())

let create view =
  {
    view;
    previous_views = [];
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

let make_status_bar_style has_focus =
  if has_focus then
    Common_style.focus
  else
    Style.make ~bg: White ~fg: Black ()

(* Render status bar for a panel of kind [File]. *)
let render_file_status_bar has_focus (frame: Render.frame) (view: File.view) ~x ~y ~w =
  let file = view.file in
  let style = make_status_bar_style has_focus in

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
      Common_style.focus
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
  let rec loop index (choices: File.choice_item list) =
    let y = y + h - 1 - index in
    if y >= 0 then
      match choices with
        | [] ->
            ()
        | (kind, label) :: tail ->
            let style =
              match index = choice, kind with
                | false, Other ->
                    Style.default
                | true, Other ->
                    Style.make ~fg: Black ~bg: White ()
                | false, Recent ->
                    Style.fg Cyan
                | true, Recent ->
                    Style.make ~fg: Black ~bg: Cyan ()
                | false, Directory ->
                    Style.bold ~fg: Blue ()
                | true, Directory ->
                    Style.bold ~fg: Black ~bg: Blue ()
            in
            Render.text ~style frame x y w label;
            loop (index + 1) tail
  in
  loop 0 choices

let render focused_panel (frame: Render.frame) panel ~x ~y ~w ~h =
  let has_focus = panel == focused_panel in
  let view = panel.view in

  let render_view has_focus ~x ~y ~w ~h =
    let cursor_style =
      if has_focus then
        Common_style.focus
      else
        Style.make ~fg: Black ~bg: Yellow ()
    in
    render_view
      ~style: Style.default
      ~cursor_style
      ~selection_style: (Style.make ~fg: Black ~bg: White ())
      frame view ~x ~y ~w ~h
  in

  (* Render prompt, if any. *)
  let has_focus, h =
    match panel.view.prompt with
      | None ->
          has_focus, h
      | Some { prompt_text; prompt_view } ->
          render_prompt has_focus frame prompt_view prompt_text ~x ~y: (y + h - 1) ~w;
          false, h - 1
  in
  let has_focus, h =
    match panel.view.search with
      | None ->
          has_focus, h
      | Some { search_view } ->
          let backwards, case_sensitive =
            match search_view.kind with
              | Search { backwards; case_sensitive } -> backwards, case_sensitive
              | _ -> false, false (* weird *)
          in
          render_prompt has_focus frame search_view
            (
              match backwards, case_sensitive with
                | false, false -> "Search for: "
                | true, false -> "Search backwards for: ";
                | false, true -> "(Case-Sensitive) Search for: ";
                | true, true -> "(Case-Sensitive) Search backwards for: ";
            )
            ~x ~y: (y + h - 1) ~w;
          false, h - 1
  in
  match panel.view.kind with
    | Prompt | Search _ ->
        (* Do not render these directly, always render their parent view. *)
        invalid_arg "Panel.render: Prompt"

    | File ->
        render_view has_focus ~x ~y ~w ~h: (h - 1);
        render_file_status_bar has_focus frame view ~x ~y: (y + h - 1) ~w

    | List_choice { choice_prompt_text; choices; choice } ->
        let filter = Text.to_string view.file.text in
        let choices = filter_choices filter choices in
        render_choice_list frame choices choice ~x ~y ~w ~h: (h - 1);
        render_prompt has_focus frame view choice_prompt_text ~x ~y: (y + h - 1) ~w

    | Help { topic } ->
        render_view has_focus ~x ~y ~w ~h: (h - 1);
        let style = make_status_bar_style has_focus in
        Render.text ~style frame x (y + h - 1) w ("Help (" ^ topic ^ ") -- Press Q to close.")
