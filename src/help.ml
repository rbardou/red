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
    add_parameter: string -> unit;
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

let make topic (state: State.t) (f: maker -> unit) =
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

  let add_parameter name =
    add ~style: (Style.make ~fg: Green ()) (String.uppercase_ascii name)
  in

  f { add; nl; line; par; header; add_link; see_also; add_parameter };

  topic, !result_text, !result_style, !result_links

let page_exists name =
  match String_map.find name !commands with
    | exception Not_found ->
        false
    | command ->
        true

let add_binding { add; add_link; nl } key (command: Redl.Ast.file) =
  let key = Key.display key in
  let padding = 24 - List.length (Utf8.split_runes key) in
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
  nl ()

let add_bindings bindings ({ line } as maker) =
  if Key.Map.is_empty bindings then
    line "(no bindings)"
  else
    Key.Map.iter (add_binding maker) bindings

let bindings_page state =
  make "bindings" state @@ fun ({ header } as maker) ->

  (* Local bindings. *)
  let context = State.get_context state in
  header (
    match context with
      | Global -> assert false (* not a local context *)
      | File -> "The following bindings are available because you are editing a file:"
      | Prompt -> "The following bindings are available because you are in a prompt:"
      | Search -> "The following bindings are available because you are searching:"
      | List_choice -> "The following bindings are available because you are choosing from a list:"
      | Help -> "The following bindings are available because you are in the help panel:"
  );
  add_bindings (get_bindings context) maker;

  (* Global bindings. *)
  header "The following bindings are available globally:";
  add_bindings (get_bindings Global) maker

let command_page (command: command) state =
  make command.name state @@ fun ({ add; add_parameter; nl; header; par; line } as maker) ->

  header "COMMAND";
  let add_variant parameters =
    add command.name;
    let add_parameter (name, Redl.Typing.Type typ) =
      add " (";
      add_parameter name;
      add ": ";
      add (Redl.Typing.show_type false typ);
      add ")";
    in
    List.iter add_parameter parameters;
    nl ();
  in
  List.iter add_variant command.variants;

  header "DESCRIPTION";
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
    (* Get bindings where this command appears. *)
    let bindings = Key.Map.filter (fun _ -> Redl.Ast.file_uses_command command.name) bindings in

    (* Show these bindings. *)
    if not (Key.Map.is_empty bindings) then (
      exists := true;

      (* Show context. *)
      par ();
      (
        match context with
          | Global -> line "Globally:"
          | File -> line "In a file:"
          | Search -> line "When searching:"
          | Prompt -> line "In a prompt:"
          | List_choice -> line "When choosing from a list:"
          | Help -> line "In the help panel:"
      );
      par ();

      (* Show bindings. *)
      add_bindings bindings maker
    )
  in
  State.Context_map.iter display_binding !bindings;
  if not !exists then line "This command is not bound in any context."

let page (name: string) state =
  match name with
    | "bindings" ->
        bindings_page state
    | _ ->
        match String_map.find name !commands with
          | exception Not_found ->
              make name state @@ fun { add; nl } ->
              add "Help page not found: "; add name; nl ()
          | command ->
              command_page command state

let command_page_list () =
  String_map.keys !commands

let make_page_list () =
  "bindings" :: command_page_list ()
