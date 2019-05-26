open Misc

module Context =
struct
  type t =
    | Global
    | File
    | Prompt
    | List_choice
    | Help

  let compare = Pervasives.compare

  let list =
    [
      Global;
      File;
      Prompt;
      List_choice;
      Help;
    ]
end

module Context_map = Map.Make (Context)

type command =
  {
    name: string;
    run: t -> unit;
  }

and t =
  {
    mutable layout: Layout.t;
    mutable focus: Panel.t;

    (* Global bindings can be overridden by local bindings. *)
    mutable bindings: bindings Context_map.t;

    (* Files to check for modification before exiting.
       Also files that should not be reopened (reuse them instead). *)
    mutable files: File.t list;

    clipboard: Clipboard.t;
  }

and bindings = command Key.Map.t

type command_definitions = command String_map.t

let create ?focus layout =
  let focus =
    match focus with
      | None ->
          Layout.get_main_panel layout
      | Some focus ->
          focus
  in
  {
    layout;
    focus;
    bindings = Context_map.empty;
    clipboard = { text = Text.empty };
    files = [];
  }

exception Exit
exception Abort

let abort ?exn reason =
  Log.error ?exn "%s" reason;
  raise Abort

let abort ?exn x = Printf.ksprintf (abort ?exn) x

let get_context_bindings context state =
  match Context_map.find context state.bindings with
    | exception Not_found ->
        Key.Map.empty
    | bindings ->
        bindings

let get_local_bindings state =
  match state.focus.view.kind with
    | File ->
        get_context_bindings File state
    | Prompt _ ->
        get_context_bindings Prompt state
    | List_choice _ ->
        get_context_bindings List_choice state
    | Help _ ->
        get_context_bindings Help state

let get_global_bindings state =
  get_context_bindings Global state

let on_key_press state (key: Key.t) =
  let catch f x =
    try
      f x
    with
      | Abort ->
          ()
      | System.Error reason ->
          Log.error "%s" reason
  in
  match Key.Map.find key (get_local_bindings state) with
    | { run } ->
        catch run state
    | exception Not_found ->
        match Key.Map.find key (get_global_bindings state) with
          | { run } ->
              catch run state
          | exception Not_found ->
              match Key.symbol key with
                | ASCII char ->
                    catch (File.replace_selection_by_character (String.make 1 char)) state.focus.view
                | Unicode character ->
                    catch (File.replace_selection_by_character character) state.focus.view
                | Control ->
                    Log.info "unbound key: %s" (Key.show key)

let render state frame =
  Layout.render
    (Panel.render state.focus)
    frame
    (Render.width frame)
    (Render.height frame)
    state.layout

let set_layout state layout =
  state.layout <- layout

let create_file state name text =
  let file = File.create name text in
  state.files <- file :: state.files;
  file

let create_file_loading state filename =
  let file = create_file state filename Text.empty in
  File.load file filename;
  file

let remove_panel panel state =
  match Layout.remove_panel panel state.layout with
    | None ->
        abort "cannot remove last panel"
    | Some (new_layout, next_panel) ->
        set_layout state new_layout;
        state.focus <- next_panel

let set_focus state focus =
  (
    match state.focus.view.kind with
      | Prompt _ ->
          remove_panel state.focus state
      | File | List_choice _ | Help _ ->
          ()
  );
  state.focus <- focus
