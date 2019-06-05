type mark =
  {
    mutable x: int;
    mutable y: int;
  }

type cursor =
  {
    selection_start: mark;
    position: mark;
    mutable preferred_x: int;
    clipboard: Clipboard.t;
    search_start: mark;
  }

(* Test whether a mark is before another mark. *)
let (<%) m1 m2 = m1.y < m2.y || (m1.y = m2.y && m1.x < m2.x)
let (<=%) m1 m2 = m1.y < m2.y || (m1.y = m2.y && m1.x <= m2.x)

let selection_boundaries cursor =
  if cursor.position <% cursor.selection_start then
    cursor.position, cursor.selection_start
  else
    cursor.selection_start, cursor.position

let cursor_is_in_selection x y cursor =
  let left, right = selection_boundaries cursor in
  let xy = { x; y } in
  left <=% xy && xy <% right

let selection_is_empty cursor =
  cursor.position.x = cursor.selection_start.x &&
  cursor.position.y = cursor.selection_start.y

type loading =
  | No
  | File of { loaded: int; size: int; sub_strings_rev: (string * int) list }
  | Process of string

type 'state stylist_status =
  (* Notĥing to do, style is already up-to-date. *)
  | Up_to_date

  (* Going backwards, looking for the beginning of the token which we just modified. *)
  | Search_beginning of {
      next_x: int; (* next position to read (backwards), must start one character before edits *)
      next_y: int;
      end_x: int; (* must update at least until this position *)
      end_y: int;
    }

  (* Going forwards, parsing tokens. *)
  | Parse of {
      start_x: int; (* current token position *)
      start_y: int;
      state: 'state; (* state before next *)
      next_x: int; (* next position to read *)
      next_y: int;
      end_x: int; (* must update at least until this position *)
      end_y: int;
    }

type 'state stylist_module =
  {
    (* Initial state, i.e. the state before reading position [(0, 0)]. *)
    start: 'state;

    (* Test whether two states are similar enough to stop parsing.
       Should most often be [(=)]. *)
    equivalent: 'state -> 'state -> bool;

    (* Update the state of a parser after reading one character.

       Usage: [add_char char continue start state]

       Shall call [continue] when the current token is not finished, i.e. [char] is part of it.
       The state can change though; its new value shall be given to [continue].

       Shall call [start] when the current token finishes, i.e. [char] is part of a new token.
       The style of the token which just finished shall be given to [start], as well as the new state. *)
    add_char: 'a. Character.t -> 'state -> ('state -> 'a) -> (Style.t -> 'state -> 'a) -> 'a;

    (* Get the color of the final token. *)
    end_of_file: 'state -> Style.t;
  }

type packed_stylist_module = Stylist_module: 'a stylist_module -> packed_stylist_module

type 'state stylist =
  {
    stylist_module: 'state stylist_module;

    (* For each position [(x, y)], [state] contains the state
       just before the character at this position is added.

       Note that we do not store the state before newlines characters,
       as we maintain [state] to have the same shape as the file [text]. *)
    mutable state: 'state Text.t;

    (* Stylists run concurrently in their own spawn group.
       Only one of them may run at any given time, until the whole region denoted by [need_to_update_*] fields
       has been updated.
       Status is [None] if style is up to date. *)
    mutable status: 'state stylist_status;

    (* If a stylist is running in the background, [group] contains its group. *)
    mutable group: Spawn.group option;
  }

type packed_stylist = Stylist: 'a stylist -> packed_stylist

module History_context =
struct
  type t =
    | Command
    | External_command
    | Help_page

  let compare = Pervasives.compare
end

type choice_kind =
  | Other
  | Recent
  | Directory
  | Modified
  | Recent_modified

type choice_item = choice_kind * string

type t =
  {
    mutable views: view list;
    mutable text: Character.t Text.t;
    mutable modified: bool;
    mutable name: string;
    mutable filename: string option;
    mutable read_only: bool;

    mutable undo_stack: undo list;
    mutable redo_stack: undo list;

    (* If [loading] is [Some (loaded, size, sub_strings_rev)], only [loaded] bytes out of [size]
       have been loaded, and [text] is read-only. *)
    mutable loading: loading;
    mutable spawn_group: Spawn.group option;

    mutable process_status: Unix.process_status option;
    mutable live_process_ids: int list;
    mutable open_file_descriptors: Unix.file_descr list;
    mutable on_edit: unit -> unit;
    history_context: History_context.t option;
  }

and view =
  {
    kind: view_kind;
    file: t;
    mutable scroll_x: int;
    mutable scroll_y: int;
    mutable width: int; (* set when rendering *)
    mutable height: int; (* set when rendering *)
    mutable marks: mark list;
    mutable cursors: cursor list;
    mutable style: Style.t Text.t;
    mutable stylist: packed_stylist option;
    mutable prompt: prompt option;
    mutable search: search option;
  }

and prompt =
  {
    prompt_text: string;
    validate_prompt: string -> unit;
    prompt_view: view;
  }

and search =
  {
    search_view: view;
  }

and undo =
  {
    undo_text: Character.t Text.t;
    undo_modified: bool;
    undo_views: undo_view list;
  }

and undo_view =
  {
    undo_view: view;
    undo_scroll_x: int;
    undo_scroll_y: int;
    undo_marks: undo_mark list;
    undo_style: Style.t Text.t;
    undo_stylist: packed_undo_stylist option;
  }

and undo_mark =
  {
    undo_mark: mark;
    undo_x: int;
    undo_y: int;
  }

and packed_undo_stylist = Undo_stylist: 'a undo_stylist -> packed_undo_stylist

and 'state undo_stylist =
  {
    undo_stylist_module: 'state stylist_module;
    undo_state: 'state Text.t;
    undo_status: 'state stylist_status;
  }

and choice =
  {
    choice_prompt_text: string;
    validate_choice: string -> unit;
    choices: choice_item list;
    mutable choice: int; (* among choices that match the filter *)
  }

and help =
  {
    topic: string;
    links: string option Text.t;
  }

and view_kind =
  | File
  | Prompt
  | Search of { backwards: bool; case_sensitive: bool }
  | List_choice of choice
  | Help of help

let get_name file =
  file.name

let has_name name file =
  file.name = name

let foreach_view file f =
  List.iter f file.views

let foreach_cursor view f =
  List.iter f view.cursors

let recenter_y view cursor =
  let scroll_for_last_line =
    Text.get_line_count view.file.text - view.height
  in
  view.scroll_y <- max 0 (min scroll_for_last_line (cursor.position.y - view.height / 2))

let recenter_x view cursor =
  view.scroll_x <- max 0 (cursor.position.x - view.width / 2)

let if_only_one_cursor view f =
  match view.cursors with
    | [ cursor ] ->
        f cursor
    | _ ->
        ()

let recenter_x_if_needed view =
  if_only_one_cursor view @@ fun cursor ->
  if cursor.position.x < view.scroll_x || cursor.position.x >= view.scroll_x + view.width - 1 then
    recenter_x view cursor

let recenter_y_if_needed view =
  if_only_one_cursor view @@ fun cursor ->
  if cursor.position.y < view.scroll_y || cursor.position.y >= view.scroll_y + view.height - 1 then
    recenter_y view cursor

let recenter_if_needed view =
  recenter_x_if_needed view;
  recenter_y_if_needed view

let max_update_style_iteration_count = 1000

let update_style_from_status view stylist status =
  let rec search_beginning iteration next_x next_y end_x end_y =
    if iteration >= max_update_style_iteration_count then
      Search_beginning { next_x; next_y; end_x; end_y }

    else
      let stylist_module = stylist.stylist_module in
      let text = view.file.text in

      (* Get previous position. *)
      let previous_position =
        if next_x > 0 then
          Some (next_x - 1, next_y)
        else if next_y > 0 then
          (* Skip newline characters until there is a non-empty line.
             A file with very large amounts of sequential empty lines means that we block for a while,
             but the alternative is to have [stylist.state] contain one more character per line and it
             is annoying to maintain. *)
          (* TODO: we can probably fix easily this actually *)
          let rec find_non_empty_line y =
            if y < 0 then
              None
            else
              let length = Text.get_line_length y text in
              if length = 0 then
                find_non_empty_line (y - 1)
              else
                Some (length - 1, y)
          in
          find_non_empty_line (next_y - 1)
        else
          None
      in

      match previous_position with
        | None ->
            (* If no previous position, just parse from the start. *)
            parse_forwards (iteration + 1) 0 0 stylist_module.start 0 0 end_x end_y

        | Some (previous_x, previous_y) ->
            (* Get character and state at previous position. *)
            let previous_character =
              match Text.get previous_x previous_y text with
                | None ->
                    Log.error "Text.get %d %d text: invalid position" previous_x previous_y;
                    assert false (* previous_position should have been None *)
                | Some character ->
                    character
            in
            let previous_state =
              match Text.get previous_x previous_y stylist.state with
                | None ->
                    Log.error "Text.get %d %d stylist.state: invalid position" previous_x previous_y;
                    assert false (* previous_position should have been None *)
                | Some state ->
                    state
            in

            (* See if previous position is the start of a token. *)
            stylist_module.add_char previous_character previous_state
              (
                fun _ ->
                  (* If not, continue to search for the beginning of a token. *)
                  search_beginning (iteration + 1) previous_x previous_y end_x end_y
              )
              (
                fun _ _ ->
                  (* If it is, start parsing from previous position. *)
                  parse_forwards (iteration + 1) previous_x previous_y previous_state previous_x previous_y end_x end_y
              )

  and parse_forwards iteration start_x start_y state next_x next_y end_x end_y =
    if iteration >= max_update_style_iteration_count then
      Parse { start_x; start_y; state; next_x; next_y; end_x; end_y }

    else
      let stylist_module = stylist.stylist_module in
      let text = view.file.text in

      let set_style style =
        view.style <- Text.map_sub ~x1: start_x ~y1: start_y ~x2: (next_x - 1) ~y2: next_y (fun _ -> style) view.style
      in

      let end_token old_state new_state new_next_x new_next_y =
        let can_stop_here =
          match old_state with
            | None ->
                false
            | Some old_state ->
                stylist_module.equivalent state old_state &&
                { x = end_x; y = end_y } <% { x = next_x; y = next_y }
        in
        if can_stop_here then
          Up_to_date
        else
          parse_forwards (iteration + 1) next_x next_y new_state new_next_x new_next_y end_x end_y
      in

      let feed_stylist_with_character old_state character new_next_x new_next_y =
        stylist_module.add_char character state
          (
            (* Token continues. *)
            fun state ->
              parse_forwards (iteration + 1) start_x start_y state new_next_x new_next_y end_x end_y
          )
          (
            (* Start of a new token. *)
            fun style state ->
              set_style style;
              end_token old_state state new_next_x new_next_y
          )
      in

      let line_count = Text.get_line_count text in
      if next_y >= line_count then
        (* End of file. *)
        (
          set_style (stylist_module.end_of_file state);
          Up_to_date
        )
      else
        let line_length = Text.get_line_length next_y text in
        if next_x >= line_length then
          (* End of line. *)
          feed_stylist_with_character None "\n" 0 (next_y + 1)
        else (
          (* Regular character. *)
          let old_state = Text.get next_x next_y stylist.state in
          stylist.state <- Text.set next_x next_y state stylist.state;
          let character =
            match Text.get next_x next_y text with
              | None ->
                  assert false (* We just checked the position above. *)
              | Some character ->
                  character
          in
          feed_stylist_with_character old_state character (next_x + 1) next_y
        )

  in
  match status with
    | Up_to_date ->
        Up_to_date
    | Search_beginning { next_x; next_y; end_x; end_y } ->
        search_beginning 0 next_x next_y end_x end_y
    | Parse { start_x; start_y; state; next_x; next_y; end_x; end_y } ->
        parse_forwards 0 start_x start_y state next_x next_y end_x end_y

let rec update_style view =
  match view.stylist with
    | None ->
        ()
    | Some (Stylist stylist) ->
        stylist.status <- update_style_from_status view stylist stylist.status;
        match stylist.status with
          | Up_to_date ->
              ()
          | _ ->
              match stylist.group with
                | None ->
                    let group = Spawn.group () in
                    stylist.group <- Some group;
                    Spawn.task ~group @@ fun () ->
                    Spawn.kill group;
                    stylist.group <- None;
                    update_style view
                | Some _ ->
                    ()

(* Update the range that a stylist must update, to include the given range. *)
let update_stylist_range ~x1 ~y1 ~x2 ~y2 view =
  match view.stylist with
    | None ->
        ()
    | Some (Stylist stylist) ->
        match stylist.status with
          | Up_to_date ->
              (* Start running stylist in background. *)
              stylist.status <-
                Search_beginning {
                  next_x = x1;
                  next_y = y1;
                  end_x = x2;
                  end_y = y2;
                };
              update_style view

          | Search_beginning { next_x = old_x1; next_y = old_y1; end_x = old_x2; end_y = old_y2 }
          | Parse { start_x = old_x1; start_y = old_y1; end_x = old_x2; end_y = old_y2 } ->
              let xy1 = { x = x1; y = y1 } in
              let xy2 = { x = x2; y = y2 } in
              let old_xy1 = { x = old_x1; y = old_y1 } in
              let old_xy2 = { x = old_x2; y = old_y2 } in
              let new_xy1 = if xy1 <% old_xy1 then xy1 else old_xy1 in
              let new_xy2 = if xy2 <% old_xy2 then old_xy2 else xy2 in
              stylist.status <-
                Search_beginning {
                  next_x = new_xy1.x;
                  next_y = new_xy1.y;
                  end_x = new_xy2.x;
                  end_y = new_xy2.y;
                }

let set_stylist_module view stylist_module =
  (* Kill old stylist. *)
  (
    match view.stylist with
      | None ->
          ()
      | Some (Stylist stylist) ->
          match stylist.group with
            | None ->
                ()
            | Some group ->
                Spawn.kill group;
                stylist.group <- None
  );

  (* Replace by new stylist. *)
  match stylist_module with
    | None ->
        (* TODO: reset style to default? (But incrementally.) *)
        view.stylist <- None
    | Some (Stylist_module stylist_module) ->
        let text = view.file.text in
        let last_line = Text.get_line_count text - 1 in
        let stylist =
          {
            stylist_module;
            state = Text.map (fun _ -> stylist_module.start) text;
            status =
              Parse {
                start_x = 0;
                start_y = 0;
                state = stylist_module.start;
                next_x = 0;
                next_y = 0;
                end_x = Text.get_line_length last_line text;
                end_y = last_line;
              };
            group = None;
          }
        in
        view.stylist <- Some (Stylist stylist);
        update_style view

(* Move marks after text has been inserted.

   Move marks as if [lines] lines were inserted after [x, y]
   and [characters] characters were inserted after those lines.

   For instance, adding 2 lines and 10 characters means that
   line [y] becomes the beginning of [y] up to [x], plus the first new line;
   a new line is added after that; and 10 characters are added at the beginning
   of the line which was at [y + 1] and is now at [y + 2]. *)
let update_marks_after_insert ~x ~y ~characters ~lines marks =
  let xy = { x; y } in
  let move_mark mark =
    if mark <% xy then
      (* Inserting after mark: do not move mark. *)
      ()
    else (
      (* Inserting before or at mark: move mark. *)
      if mark.y = y then
        (* Inserting on the same line as the mark; x coordinate changes.

           Example 1: inserting XXX at | (i.e. [lines] is 0 and [characters] is 3):

               -------|-------M--------

           Becomes:

               -------|XXX-------M--------

           Example 2: inserting XXX\nYYYY\nZ at | (i.e. [lines] is 2 and [characters] is 1):

               -------|-------M--------

           Becomes:

               -------|XXX\n
               YYYY\n
               Z-------M--------

           Only the length of Z (i.e. [characters]) line matters for the x coordinate,
           not the length of XXX and YYYY. *)
        (
          if lines = 0 then
            mark.x <- mark.x + characters
          else
            mark.x <- mark.x - x + characters;
          mark.y <- mark.y + lines;
        )
      else
        (* Inserting on a previous line; x coordinate does not change. *)
        mark.y <- mark.y + lines
    )
  in
  List.iter move_mark marks

(* Move marks after text has been deleted.

   Move marks as if [lines] lines were deleted after [(x, y)],
   including the end of [y], and [characters] characters were deleted
   after those lines.

   For instance, deleting 2 lines and 10 characters means that
   the end of line [y] is deleted, that line [y + 1] is deleted,
   and that the first 10 characters of line [y + 2] are deleted. *)
let update_marks_after_delete ~x ~y ~characters ~lines marks =
  (* Beginning of the deleted region. *)
  let xy = { x; y } in

  (* End of the deleted region. *)
  let xy2 =
    if lines = 0 then
      { x = x + characters; y }
    else
      { x = characters; y = y + lines }
  in

  let move_mark mark =
    (* For a given mark, either the mark is:
       - before the removed region, in which case the mark does not move;
       - inside the removed region, in which case the mark moves to the beginning of this removed region;
       - after the removed region, in which case the mark moves like when inserting but in reverse. *)
    if mark <% xy then
      (* Deleting after mark: do not move mark. *)
      ()
    else if xy <=% mark && mark <=% xy2 then
      (* Mark is inside deleted region: move mark to the beginning of the deleted region. *)
      (
        mark.x <- x;
        mark.y <- y;
      )
    else (
      (* Mark is after deleted region: move it. *)
      if mark.y = xy2.y then
        (* Deleting on the same line as the mark; x coordinate changes.

           Exemple 1: deleting XXX from a single line (i.e. [lines] is 0 and [characters] is 3):

               -------XXX-----M------

           Becomes:

               ------------M------

           Example 2: deleting XXX\nYYYY\nZ (i.e. [lines] is 2 and [characters] is 1):

               -------XXX\n
               YYYY\n
               Z-----M------

           Becomes:

               ------------M------

           Once again, only the length of the last line matters. *)
        (
          mark.x <- mark.x - characters + (if lines = 0 then 0 else x);
          mark.y <- mark.y - lines;
        )
      else
        (* Deleting lines which are strictly before the mark; x coordinate does not change. *)
        mark.y <- mark.y - lines
    )
  in
  List.iter move_mark marks

let update_views_after_insert ?(keep_marks = false) ~x ~y ~characters ~lines views =
  (* Prepare a chunk of default style to insert. *)
  let style_sub =
    match views with
      | [] ->
          Text.empty
      | view :: _ ->
          let sub_region = Text.sub_region ~x ~y ~characters ~lines view.file.text in
          Text.map (fun _ -> Style.default) sub_region
  in

  (* Compute the position of the last character which was inserted. *)
  let x2 =
    if lines = 0 then
      x + characters - 1
    else
      characters - 1
  in
  let y2 = y + lines in

  (* Update all views. *)
  let update_view view =
    if not keep_marks then update_marks_after_insert ~x ~y ~characters ~lines view.marks;
    view.style <- Text.insert_text ~x ~y ~sub: style_sub view.style;
    (
      match view.stylist with
        | None ->
            ()
        | Some (Stylist stylist) ->
            let stylist_state_sub =
              match views with
                | [] ->
                    Text.empty
                | view :: _ ->
                    let start = stylist.stylist_module.start in
                    Text.map (fun _ -> start) style_sub
            in
            stylist.state <- Text.insert_text ~x ~y ~sub: stylist_state_sub stylist.state
    );
    update_stylist_range ~x1: x ~y1: y ~x2 ~y2 view;
  in
  List.iter update_view views

let update_views_after_delete ~x ~y ~characters ~lines views =
  let update_view view =
    update_marks_after_delete ~x ~y ~characters ~lines view.marks;
    view.style <- Text.delete_region ~x ~y ~characters ~lines view.style;
    (
      match view.stylist with
        | None ->
            ()
        | Some (Stylist stylist) ->
            stylist.state <- Text.delete_region ~x ~y ~characters ~lines stylist.state
    );
    (* TODO: we could optimize by reducing the "need to update" region by the region which was deleted
       if those two regions intersects. *)
    update_stylist_range ~x1: x ~y1: y ~x2: x ~y2: y view;
  in
  List.iter update_view views

(* If [file_descr] is in [file.open_file_descriptors], close it. *)
let close_file_descriptor file file_descr =
  let rec find acc list =
    match list with
      | [] ->
          List.rev acc
      | head :: tail ->
          if head = file_descr then
            (
              Unix.close head; (* TODO: error handling *)
              List.rev_append acc tail
            )
          else
            find (head :: acc) tail
  in
  file.open_file_descriptors <- find [] file.open_file_descriptors

(* Wait for [pid] to terminate, in the background. *)
let rec spawn_wait_pid name file pid =
  let waitpid_result_pid, status = Unix.waitpid [ WNOHANG ] pid in
  if waitpid_result_pid = 0 then
    (* Process is still running. Usually, processes are already dead when their output is closed though. *)
    (* TODO: better way to wait than Spawn.sleep ?? *)
    Spawn.sleep 1. @@ fun () ->
    spawn_wait_pid name file pid
  else
    file.process_status <- Some status

(* If [pid] is in [file.live_process_ids], spawn a process to collect it. *)
let wait_pid name file pid =
  let rec find acc list =
    match list with
      | [] ->
          List.rev acc
      | head :: tail ->
          if head = pid then
            (
              spawn_wait_pid name file pid;
              List.rev_append acc tail
            )
          else
            find (head :: acc) tail
  in
  file.live_process_ids <- find [] file.live_process_ids

let close_all_file_descriptors file =
  List.iter Unix.close file.open_file_descriptors; (* TODO: error handling *)
  file.open_file_descriptors <- []

let wait_all_pids file =
  List.iter (spawn_wait_pid "" file) file.live_process_ids; (* TODO: error handling *)
  file.live_process_ids <- []

let create_cursor x y =
  {
    selection_start = { x; y };
    position = { x; y };
    preferred_x = x;
    clipboard = { text = Text.empty };
    search_start = { x; y };
  }

(* Created by [Command] because of circular dependency issues. *)
let choose_stylist_automatically = ref (fun _ -> Log.error "set_stylist is not initialized"; assert false)

let create_view kind file =
  let cursor = create_cursor 0 0 in
  let view =
    {
      kind;
      file;
      scroll_x = 0;
      scroll_y = 0;
      width = 80;
      height = 40;
      marks = [ cursor.selection_start; cursor.position ];
      cursors = [ cursor ];
      style = Text.map (fun _ -> Style.default) file.text;
      stylist = None;
      prompt = None;
      search = None;
    }
  in
  file.views <- view :: file.views;
  !choose_stylist_automatically view;
  view

(* You may want to use [State.create_file] instead. *)
let create ?(read_only = false) ?history name text =
  {
    views = [];
    text;
    modified = false;
    name;
    filename = None;
    read_only;
    loading = No;
    undo_stack = [];
    redo_stack = [];
    spawn_group = None;
    process_status = None;
    live_process_ids = [];
    open_file_descriptors = [];
    on_edit = (fun () -> ());
    history_context = history;
  }

(* Note: this does not kill stylists. *)
let kill_spawn_group file =
  match file.spawn_group with
    | None ->
        ()
    | Some group ->
        Spawn.kill group;
        file.spawn_group <- None

let set_filename file filename =
  file.filename <- filename;
  foreach_view file !choose_stylist_automatically

(* Reset a file to an empty text as if it was created by [create].
   Keep views, but reset them as well. *)
let reset file =
  (* Reset file. *)
  file.text <- Text.empty;
  file.loading <- No;
  file.modified <- false;
  file.undo_stack <- [];
  file.redo_stack <- [];
  kill_spawn_group file;
  file.process_status <- None;
  close_all_file_descriptors file;
  wait_all_pids file;
  set_filename file None;

  (* Reset views. *)
  foreach_view file @@ fun view ->
  view.scroll_x <- 0;
  view.scroll_y <- 0;
  let cursor = create_cursor 0 0 in
  view.marks <- [ cursor.selection_start; cursor.position ];
  view.cursors <- [ cursor ];
  view.style <- Text.empty;
  set_stylist_module view None

let create_spawn_group file =
  kill_spawn_group file;
  let group = Spawn.group () in
  file.spawn_group <- Some group;
  group

let load file filename =
  let file_descr =
    (* TODO: error handling (e.g. file does not exist) *)
    Unix.openfile filename [ O_RDONLY ] 0o640
  in
  let size =
    (* TODO: error handling *)
    (Unix.fstat file_descr).st_size
  in
  reset file;
  set_filename file (Some filename);
  file.loading <- File { loaded = 0; size; sub_strings_rev = [] };
  file.open_file_descriptors <- file_descr :: file.open_file_descriptors;
  let group = create_spawn_group file in

  (* Start loading. *)
  let rec load () =
    Spawn.on_read ~group file_descr @@ fun () ->
    match file.loading with
      | No | Process _ ->
          assert false (* did someone else temper with this file? *)
      | File { loaded; size; sub_strings_rev } ->
          let bytes = Bytes.create 8192 in
          let len = Unix.read file_descr bytes 0 8192 in
          if len = 0 then
            (* TODO: [Text.of_utf8_substrings_offset_0] can block for a while.
               Replace it by some kind of iterative parser. *)
            (
              close_file_descriptor file file_descr;
              file.loading <- No;
              let text = Text.of_utf8_substrings_offset_0 (List.rev sub_strings_rev) in
              file.text <- text;
              let lines = Text.get_line_count text - 1 in
              let characters = Text.get_line_length lines text in
              update_views_after_insert ~keep_marks: true ~x: 0 ~y: 0 ~characters ~lines file.views
            )
          else
            let sub_strings_rev = (Bytes.unsafe_to_string bytes, len) :: sub_strings_rev in
            file.loading <- File { loaded = loaded + len; size; sub_strings_rev };
            load ()
  in
  load ()

let create_process file program arguments =
  reset file;
  set_filename file None;
  file.loading <- Process program;
  let group = create_spawn_group file in

  (* Create process. *)
  (* TODO: error handling for all Unix commands. *)
  let in_exit, in_entry = Unix.pipe () in
  let out_exit, out_entry = Unix.pipe () in
  Unix.set_close_on_exec in_entry;
  Unix.set_close_on_exec out_exit;
  let pid =
    Unix.create_process
      program
      (Array.of_list (program :: arguments))
      in_exit
      out_entry
      out_entry
  in
  Unix.close in_exit;
  Unix.close out_entry;
  Unix.close in_entry; (* We won't send inputs to the process. *)
  file.open_file_descriptors <- out_exit :: file.open_file_descriptors;
  file.live_process_ids <- pid :: file.live_process_ids;

  (* Start reading. *)
  let rec read alive (utf8_parser: Utf8.parser_state) file_descr =
    Spawn.on_read ~group file_descr @@ fun () ->
    let bytes = Bytes.create 8192 in
    let len = Unix.read file_descr bytes 0 8192 in
    if len = 0 then
      (
        file.loading <- No;
        close_file_descriptor file file_descr;
        wait_pid program file pid
      )
    else
      (
        let y = Text.get_line_count file.text - 1 in
        let x = Text.get_line_length y file.text in
        let inserted_characters = ref 0 in
        let inserted_lines = ref 0 in
        let rec parse_bytes utf8_parser i =
          if i >= len then
            read alive utf8_parser file_descr
          else
            match Utf8.add_char (Bytes.get bytes i) utf8_parser with
              | Completed_ASCII char ->
                  if char = '\n' then
                    (
                      inserted_characters := 0;
                      incr inserted_lines;
                    )
                  else
                    incr inserted_characters;
                  file.text <- Text.append_character (Character.of_ascii char) file.text;
                  parse_bytes Started (i + 1)
              | Completed_Unicode character ->
                  incr inserted_characters;
                  file.text <- Text.append_character character file.text;
                  parse_bytes Started (i + 1)
              | utf8_parser ->
                  parse_bytes utf8_parser (i + 1)
        in
        parse_bytes utf8_parser 0;
        update_views_after_insert ~x ~y ~characters: !inserted_characters ~lines: !inserted_lines file.views;
        foreach_view file recenter_y_if_needed
      )
  in
  read true Started out_exit

let is_read_only file =
  file.read_only ||
  match file.loading with
    | No ->
        false
    | File _ | Process _ ->
        true

(* Iterate on cursors and their clipboards.
   If there is only one cursor, use global clipboard instead of cursor clipboard. *)
let foreach_cursor_clipboard (global_clipboard: Clipboard.t) view f =
  match view.cursors with
    | [ cursor ] ->
        f global_clipboard cursor
    | cursors ->
        List.iter (fun cursor -> f cursor.clipboard cursor) cursors

let make_undo_view view =
  let make_undo_mark mark =
    {
      undo_mark = mark;
      undo_x = mark.x;
      undo_y = mark.y;
    }
  in
  {
    undo_view = view;
    undo_scroll_x = view.scroll_x;
    undo_scroll_y = view.scroll_y;
    undo_marks = List.map make_undo_mark view.marks;
    undo_style = view.style;
    undo_stylist = (
      match view.stylist with
        | None ->
            None
        | Some (Stylist stylist) ->
            Some (
              Undo_stylist {
                undo_stylist_module = stylist.stylist_module;
                undo_state = stylist.state;
                undo_status = stylist.status;
              }
            )
    );
  }

let make_undo file =
  {
    undo_text = file.text;
    undo_modified = file.modified;
    undo_views = List.map make_undo_view file.views;
  }

let set_text file text =
  if is_read_only file then
    invalid_arg "set_text: file is read-only"
  else (
    file.text <- text;
    file.modified <- true;
  )

let set_cursors view cursors =
  let marks =
    cursors
    |> List.map (fun cursor -> [ cursor.selection_start; cursor.position ])
    |> List.flatten
  in
  view.cursors <- cursors;
  view.marks <- marks

let delete_selection view cursor =
  (* Compute region. *)
  let left, right = selection_boundaries cursor in
  let { x; y } = left in
  let lines, characters =
    if right.y = y then
      (* No line split in selection. *)
      0, right.x - x
    else
      right.y - y, right.x
  in

  (* Delete selection. *)
  set_text view.file (Text.delete_region ~x ~y ~characters ~lines view.file.text);
  update_views_after_delete ~x ~y ~characters ~lines view.file.views

let insert_character (character: Character.t) view cursor =
  set_text view.file (Text.insert cursor.position.x cursor.position.y character view.file.text);
  update_views_after_insert ~x: cursor.position.x ~y: cursor.position.y ~characters: 1 ~lines: 0 view.file.views

let insert_new_line view cursor =
  set_text view.file (Text.insert_new_line cursor.position.x cursor.position.y view.file.text);
  update_views_after_insert ~x: cursor.position.x ~y: cursor.position.y ~characters: 0 ~lines: 1 view.file.views

let delete_character view cursor =
  let { x; y } = cursor.position in
  let text = view.file.text in
  let length = Text.get_line_length y text in
  let characters, lines = if x < length then 1, 0 else 0, 1 in
  set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
  update_views_after_delete ~x ~y ~characters ~lines view.file.views

let delete_character_backwards view cursor =
  let { x; y } = cursor.position in
  if x > 0 then
    let text = view.file.text in
    let x = x - 1 in
    let characters = 1 in
    let lines = 0 in
    set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
    update_views_after_delete ~x ~y ~characters ~lines view.file.views
  else if y > 0 then
    let text = view.file.text in
    let y = y - 1 in
    let x = Text.get_line_length y text in
    let characters = 0 in
    let lines = 1 in
    set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
    update_views_after_delete ~x ~y ~characters ~lines view.file.views

let reset_preferred_x file =
  foreach_view file @@ fun view ->
  foreach_cursor view @@ fun cursor ->
  cursor.preferred_x <- cursor.position.x

let edit save_undo file f =
  if is_read_only file then
    Log.info "Buffer is read-only."
  else (
    if save_undo then
      let undo = make_undo file in
      f ();
      file.undo_stack <- undo :: file.undo_stack;
      file.redo_stack <- []
    else
      f ();
    reset_preferred_x file;

    (
      foreach_view file @@ fun view ->
      match view.kind with
        | List_choice choice ->
            choice.choice <- -1
        | _ ->
            ()
    );
    file.on_edit ()
  )

let replace_selection_by_character character view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_character character view cursor

let replace_selection_by_new_line view =
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  delete_selection view cursor;
  insert_new_line view cursor

let delete_selection_or_character view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character view cursor
  else
    delete_selection view cursor

let delete_selection_or_character_backwards view =
  (* TODO: false if consecutive *)
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  if selection_is_empty cursor then
    delete_character_backwards view cursor
  else
    delete_selection view cursor

let delete_from_cursors view get_other_position =
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->
  let text = view.file.text in

  (* Get start and end positions in the right order. *)
  let x, y, x2, y2 =
    let position = cursor.position in
    let other_x, other_y = get_other_position text cursor in
    let other_position = { x = other_x; y = other_y } in
    if position <=% other_position then
      position.x, position.y, other_x, other_y
    else
      other_x, other_y, position.x, position.y
  in

  (* Compute the number of characters and lines to delete from (x, y), knowing that y <= y2. *)
  let characters, lines =
    if y = y2 then
      x2 - x, 0
    else
      x2, y2 - y
  in

  set_text view.file (Text.delete_region ~x ~y ~characters ~lines text);
  update_views_after_delete ~x ~y ~characters ~lines view.file.views

let get_selected_text text cursor =
  let left, right = selection_boundaries cursor in
  (* The cursor itself is not included in the selection, hence the value of x2.
     A negative value here is not an issue for Text.sub. *)
  Text.sub ~x1: left.x ~y1: left.y ~x2: (right.x - 1) ~y2: right.y text

let copy (global_clipboard: Clipboard.t) view =
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->
  clipboard.text <- get_selected_text view.file.text cursor

let cut (global_clipboard: Clipboard.t) view =
  edit true view.file @@ fun () ->
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->
  clipboard.text <- get_selected_text view.file.text cursor;
  delete_selection view cursor

let replace_selection_with_text view sub =
  edit true view.file @@ fun () ->
  foreach_cursor view @@ fun cursor ->

  (* Replace selection with text. *)
  delete_selection view cursor;
  let x = cursor.position.x in
  let y = cursor.position.y in
  set_text view.file (Text.insert_text ~x ~y ~sub view.file.text);

  (* Update marks. *)
  let lines = Text.get_line_count sub - 1 in
  let characters = Text.get_line_length lines sub in
  update_views_after_insert ~x ~y ~characters ~lines view.file.views

let paste (global_clipboard: Clipboard.t) view =
  edit true view.file @@ fun () ->
  foreach_cursor_clipboard global_clipboard view @@ fun clipboard cursor ->

  (* Replace selection with clipboard. *)
  delete_selection view cursor;
  let x = cursor.position.x in
  let y = cursor.position.y in
  let sub = clipboard.text in
  set_text view.file (Text.insert_text ~x ~y ~sub view.file.text);

  (* Update marks. *)
  let lines = Text.get_line_count sub - 1 in
  let characters = Text.get_line_length lines sub in
  update_views_after_insert ~x ~y ~characters ~lines view.file.views

let restore_view undo =
  let restore_mark undo =
    undo.undo_mark.x <- undo.undo_x;
    undo.undo_mark.y <- undo.undo_y;
  in
  undo.undo_view.scroll_x <- undo.undo_scroll_x;
  undo.undo_view.scroll_y <- undo.undo_scroll_y;
  List.iter restore_mark undo.undo_marks;
  undo.undo_view.style <- undo.undo_style;
  undo.undo_view.stylist <- (
    match undo.undo_stylist with
      | None ->
          None
      | Some (Undo_stylist undo_stylist) ->
          Some
            (
              Stylist {
                stylist_module = undo_stylist.undo_stylist_module;
                state = undo_stylist.undo_state;
                status = undo_stylist.undo_status;
                group = None;
              }
            )
  );
  update_style undo.undo_view

let restore_undo_point file undo =
  file.text <- undo.undo_text;
  file.modified <- undo.undo_modified;
  List.iter restore_view undo.undo_views

let undo file =
  edit false file @@ fun () ->
  match file.undo_stack with
    | [] ->
        Log.info "Nothing to undo."
    | undo :: remaining_stack ->
        file.undo_stack <- remaining_stack;
        file.redo_stack <- make_undo file :: file.redo_stack;
        restore_undo_point file undo

let redo file =
  edit false file @@ fun () ->
  match file.redo_stack with
    | [] ->
        Log.info "Nothing to redo."
    | undo :: remaining_stack ->
        file.undo_stack <- make_undo file :: file.undo_stack;
        file.redo_stack <- remaining_stack;
        restore_undo_point file undo

let copy_view view =
  let copy = create_view view.kind view.file in
  let undo = make_undo_view view in
  restore_view { undo with undo_view = copy };

  (* The undo / redo mechanism does not copy cursors and their marks; it reuses existing ones.
     So we need to copy them now. *)
  let marks = ref [] in
  let copy_mark (mark: mark): mark =
    let copy = { x = mark.x; y = mark.y } in
    marks := copy :: !marks;
    copy
  in
  let copy_cursor (cursor: cursor): cursor =
    {
      selection_start = copy_mark cursor.selection_start;
      position = copy_mark cursor.position;
      preferred_x = cursor.preferred_x;
      clipboard = cursor.clipboard;
      search_start = copy_mark cursor.search_start;
    }
  in
  copy.cursors <- List.map copy_cursor view.cursors;
  copy.marks <- !marks;

  copy

let get_cursor_subtext cursor text =
  let from, until = selection_boundaries cursor in
  Text.sub ~x1: from.x ~y1: from.y ~x2: (until.x - 1) ~y2: until.y text
