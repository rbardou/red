open Misc

let make (state: State.t) (f: State.Help_maker.t -> unit) =
  let result_text = ref Text.empty in
  let result_style = ref Text.empty in
  let result_links = ref Text.empty in

  let at_beginning_of_line = ref true in
  let last_line_was_empty = ref true in

  let add ?(style = Style.default) ?(link = File.HL_none) str =
    if str <> "" then (last_line_was_empty := false; at_beginning_of_line := false);
    let new_text = Text.of_utf8_string str in
    result_text := Text.concat !result_text new_text;
    result_style := Text.concat !result_style (Text.map (fun _ -> style) new_text);
    result_links := Text.concat !result_links (Text.map (fun _ -> link) new_text);
  in

  let nl () =
    if not !last_line_was_empty then (
      result_text := Text.append_new_line !result_text;
      result_style := Text.append_new_line !result_style;
      result_links := Text.append_new_line !result_links;
    );
    if !at_beginning_of_line then last_line_was_empty := true;
    at_beginning_of_line := true
  in

  let line ?style ?link str =
    add ?style ?link str;
    nl ();
  in

  let par () =
    nl ();
    nl ()
  in

  let header str =
    par ();
    line ~style: (Style.bold ()) str;
    par ();
  in

  let add_command name =
    add ~style: (Style.make ~underline: true ~fg: Cyan ()) ~link: (HL_command name) name;
  in

  let add_link (link: File.help_link) =
    match link with
      | HL_none ->
          ()
      | HL_command name ->
          add_command name
  in

  let see_also links =
    header "SEE ALSO";
    List.iter (fun link -> add "    "; add_link link; nl ()) links
  in

  f { add; nl; line; par; header; add_command; see_also };

  !result_text, !result_style, !result_links

let bindings (all_commands: State.command_definitions) state =
  make state @@ fun { add; nl; line; header; add_command } ->

  let add_binding key (command: State.command) =
    let key = Key.display key in
    let padding = 24 - String.length key in
    add (String.make padding ' ');
    add key;
    add " ";
    add_command command.name;
    nl ();
  in
  let add_bindings bindings =
    if Key.Map.is_empty bindings then
      line "(no bindings)"
    else
      Key.Map.iter add_binding bindings
  in

  (* Local bindings. *)
  header (
    match state.focus.view.kind with
      | File -> "The following bindings are available because you are editing a file:"
      | Prompt _ -> "The following bindings are available because you are in a prompt:"
      | List_choice _ -> "The following bindings are available because you are choosing from a list:"
      | Help _ -> "The following bindings are available because you are in the help panel:"
  );
  add_bindings (State.get_local_bindings state);

  (* Global bindings. *)
  header "The following bindings are available globally:";
  add_bindings (State.get_global_bindings state);

  (* Unbound commands. *)
  let unbound_commands =
    let remove (acc: State.command_definitions) (context: State.Context.t) =
      let bindings = State.get_context_bindings context state in
      Key.Map.fold (fun _ (command: State.command) acc -> String_map.remove command.name acc) bindings acc
    in
    List.fold_left remove all_commands State.Context.list
  in
  if not (String_map.is_empty unbound_commands) then (
    header "The following commands are not bound in any context:";
    let add_command _ (command: State.command) =
      add "    ";
      add_command command.name;
      nl ();
    in
    String_map.iter add_command unbound_commands
  )

let command (command: State.command) state =
  make state @@ fun ({ add; nl; header; line } as maker) ->

  header ("COMMAND: " ^ command.name);
  (
    match command.help with
      | None ->
          line "Undocumented."
      | Some help ->
          help maker
  );

  header "BINDINGS";
  let exists = ref false in
  let display_binding (context: State.Context.t) bindings =
    let bindings = Key.Map.filter (fun _ (cmd: State.command) -> cmd.name = command.name) bindings in
    let list_keys () =
      let count = Key.Map.cardinal bindings in
      let index = ref 0 in
      let add_key key _ =
        incr index;
        if !index > 1 then (
          if !index = count then add " or " else add ", "
        );
        add (Key.display key)
      in
      Key.Map.iter add_key bindings;
      add "."; nl ()
    in
    if not (Key.Map.is_empty bindings) then (
      exists := true;
      match context with
        | Global ->
            add "You can run this command using "; list_keys ()
        | File ->
            add "In a file, you can run this command using "; list_keys ()
        | Prompt ->
            add "In a prompt, you can run this command using "; list_keys ()
        | List_choice ->
            add "When choosing from a list, you can run this command using "; list_keys ()
        | Help ->
            add "In the help panel, you can run this command using "; list_keys ()
    )
  in
  State.Context_map.iter display_binding state.bindings;
  if not !exists then line "This command is not bound in any context."
