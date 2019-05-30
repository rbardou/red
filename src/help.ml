open Misc

type maker =
  {
    add: ?style: Style.t -> ?link: string -> string -> unit;
    nl: unit -> unit;
    line: ?style: Style.t -> ?link: string -> string -> unit;
    par: unit -> unit;
    header: string -> unit;
    add_link: string -> unit;
    see_also: string list -> unit;
  }

type command =
  {
    name: string;
    help: (maker -> unit) option;
    variants: (string * Redl.Typing.packed_type) list list; (* list of parameter lists *)
  }

let commands: command String_map.t ref = ref String_map.empty

let get_command name =
  match String_map.find name !commands with
    | exception Not_found ->
        { name; help = None; variants = [] }
    | command ->
        command

let overload_command name ?help parameters =
  let command = get_command name in
  let command = { command with variants = parameters :: command.variants } in
  let command =
    match help with
      | None -> command
      | Some _ -> { command with help }
  in
  commands := String_map.add name command !commands

let bindings: Redl.Ast.file Key.Map.t State.Context_map.t ref = ref State.Context_map.empty

let get_bindings context =
  match State.Context_map.find context !bindings with
    | exception Not_found ->
        Key.Map.empty
    | map ->
        map

let bind (context: State.Context.t) (key: Key.t) (command: Redl.Ast.file) =
  let context_bindings = get_bindings context in
  let context_bindings = Key.Map.add key command context_bindings in
  bindings := State.Context_map.add context context_bindings !bindings

let make (state: State.t) (f: maker -> unit) =
  let result_text = ref Text.empty in
  let result_style = ref Text.empty in
  let result_links = ref Text.empty in

  let at_beginning_of_line = ref true in
  let last_line_was_empty = ref true in

  let add ?(style = Style.default) ?link str =
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

  let add_link name =
    add ~style: (Style.make ~underline: true ~fg: Cyan ()) ~link: name name;
  in

  let see_also links =
    header "SEE ALSO";
    List.iter (fun link -> add "    "; add_link link; nl ()) links
  in

  f { add; nl; line; par; header; add_link; see_also };

  !result_text, !result_style, !result_links

let page_exists name =
  match String_map.find name !commands with
    | exception Not_found ->
        false
    | command ->
        true

let bindings_page state =
  make state @@ fun { add; nl; line; header; add_link } ->

  let add_binding key (command: Redl.Ast.file) =
    let key = Key.display key in
    let padding = 24 - String.length key in
    add (String.make padding ' ');
    add key;
    add " ";
    let add_maybe_link text =
      if page_exists text then
        add_link text
      else
        add text
    in
    Redl.Ast.out_file_flat add_maybe_link command;
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
  add_bindings (get_bindings (State.get_context state));

  (* Global bindings. *)
  header "The following bindings are available globally:";
  add_bindings (get_bindings Global);

  (* (\* Unbound commands. *\) *)
  (* let unbound_commands = *)
  (*   let remove (acc: State.command_definitions) (context: State.Context.t) = *)
  (*     let bindings = State.get_context_bindings context state in *)
  (*     Key.Map.fold (fun _ (command: State.command) acc -> String_map.remove command.name acc) bindings acc *)
  (*   in *)
  (*   List.fold_left remove all_commands State.Context.list *)
  (* in *)
  (* if not (String_map.is_empty unbound_commands) then ( *)
  (*   header "The following commands are not bound in any context:"; *)
  (*   let add_command _ (command: State.command) = *)
  (*     add "    "; *)
  (*     add_command command.name; *)
  (*     nl (); *)
  (*   in *)
  (*   String_map.iter add_command unbound_commands *)
  (* ) *)
  ()

let command_page (command: command) state =
  make state @@ fun ({ add; nl; header; line } as maker) ->

  header ("COMMAND: " ^ command.name);
  (
    match command.help with
      | None ->
          line "Undocumented."
      | Some help ->
          help maker
  );

  (* header "BINDINGS"; *)
  (* let exists = ref false in *)
  (* let display_binding (context: State.Context.t) bindings = *)
  (*   let bindings = Key.Map.filter (fun _ (cmd: State.command) -> cmd.name = command.name) bindings in *)
  (*   let list_keys () = *)
  (*     let count = Key.Map.cardinal bindings in *)
  (*     let index = ref 0 in *)
  (*     let add_key key _ = *)
  (*       incr index; *)
  (*       if !index > 1 then ( *)
  (*         if !index = count then add " or " else add ", " *)
  (*       ); *)
  (*       add (Key.display key) *)
  (*     in *)
  (*     Key.Map.iter add_key bindings; *)
  (*     add "."; nl () *)
  (*   in *)
  (*   if not (Key.Map.is_empty bindings) then ( *)
  (*     exists := true; *)
  (*     match context with *)
  (*       | Global -> *)
  (*           add "You can run this command using "; list_keys () *)
  (*       | File -> *)
  (*           add "In a file, you can run this command using "; list_keys () *)
  (*       | Prompt -> *)
  (*           add "In a prompt, you can run this command using "; list_keys () *)
  (*       | List_choice -> *)
  (*           add "When choosing from a list, you can run this command using "; list_keys () *)
  (*       | Help -> *)
  (*           add "In the help panel, you can run this command using "; list_keys () *)
  (*   ) *)
  (* in *)
  (* State.Context_map.iter display_binding state.bindings; *)
  (* if not !exists then line "This command is not bound in any context." *)
  ()

let page (name: string) state =
  match String_map.find name !commands with
    | exception Not_found ->
        make state @@ fun { add; nl } ->
        add "Help page not found: "; add name; nl ()
    | command ->
        command_page command state
