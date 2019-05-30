open Misc

let main () =
  (* Parse command-line arguments. *)
  let filenames = ref [] in
  let init_filenames = ref [] in

  let add_init filename = init_filenames := filename :: !init_filenames in
  let anon_fun filename = filenames := filename :: !filenames in
  let spec =
    Arg.align [
      "--init", String add_init, "<FILE> Parse FILE and execute its commands to initialize the editor.";
    ]
  in
  let usage_msg = "Usage: red [OPTIONS] [FILES..]" in
  Arg.parse spec anon_fun usage_msg;

  let filenames = List.rev !filenames in
  let init_filenames = List.rev !init_filenames in

  (* Load files given on the command line. *)
  let files =
    match filenames with
      | [] ->
          [ File.create "(new file)" Text.empty ]
      | _ ->
          let load_file filename =
            let file = File.create filename Text.empty in
            File.load file filename;
            file
          in
          List.map load_file filenames
  in

  (* Choose a layout depending on the number of files. *)
  let layout =
    match files with
      | [] ->
          assert false (* impossible, we created at least one empty file *)
      | [ file ] ->
          Layout.create_file file
      | [ file_1; file_2 ] ->
          Layout.split Horizontal ~sep: true (Layout.create_file file_1) (Layout.create_file file_2)
      | [ file_1; file_2; file_3 ] ->
          Layout.split Vertical
            (Layout.split Horizontal ~sep: true (Layout.create_file file_1) (Layout.create_file file_2))
            (Layout.create_file file_3)
      | file_1 :: file_2 :: file_3 :: file_4 :: _ ->
          Layout.split Vertical
            (Layout.split Horizontal ~sep: true (Layout.create_file file_1) (Layout.create_file file_2))
            (Layout.split Horizontal ~sep: true (Layout.create_file file_3) (Layout.create_file file_4))
  in

  (* Create initial state. *)
  let type_check, overload_command = Redl.init () in
  Command.setup_initial_env overload_command;
  let run_file filename = Redl.parse_file filename |> type_check |> Redl.run in
  let run_string string = Redl.parse_string string |> type_check |> Redl.run in
  let state = State.create ~run_file ~run_string layout in
  state.files <- files;

  (* Global Bindings *)
  let (=>) = Command.bind Global state in

  Alt_escape => "quit";
  F1 => "help";
  Ctrl_h => "help; scroll_down; scroll_down";
  F4 => "remove_panel"; (* TODO: better binding? *)

  Right => "move_right";
  Left => "move_left";
  Down => "move_down";
  Up => "move_up";
  End => "move_end_of_line";
  Home => "move_beginning_of_line";
  Ctrl_end => "move_end_of_file";
  Ctrl_home => "move_beginning_of_file";
  Ctrl_right => "move_right_word";
  Ctrl_left => "move_left_word";
  Ctrl_down => "move_down_paragraph";
  Ctrl_up => "move_up_paragraph";

  Ctrl_a => "select_all";
  Shift_right => "select_right";
  Shift_left => "select_left";
  Shift_down => "select_down";
  Shift_up => "select_up";
  Shift_end => "select_end_of_line";
  Shift_home => "select_beginning_of_line";
  Ctrl_shift_end => "select_end_of_file";
  Ctrl_shift_home => "select_beginning_of_file";
  Ctrl_shift_right => "select_right_word";
  Ctrl_shift_left => "select_left_word";
  Ctrl_shift_down => "select_down_paragraph";
  Ctrl_shift_up => "select_up_paragraph";

  Alt_right => "focus_right";
  Alt_left => "focus_left";
  Alt_down => "focus_down";
  Alt_up => "focus_up";

  Page_down => "scroll_down";
  Page_up => "scroll_up";

  Return => "insert_new_line";
  Delete => "delete_character";
  Ctrl_d => "delete_character";
  Backspace => "delete_character_backwards";
  Alt_double_quote => "create_cursors_from_selection";
  Ctrl_k => "delete_end_of_line";
  Alt_letter_d => "delete_end_of_word";
  Alt_backspace => "delete_beginning_of_word";

  Ctrl_c => "copy";
  Ctrl_x => "cut";
  Ctrl_v => "paste";

  Ctrl_z => "undo";
  Ctrl_y => "redo";

  (* File Bindings *)
  let (=>) = Command.bind File state in

  Ctrl_s => "save";
  Alt_ctrl_s => "save_as";
  Ctrl_o => "open";
  F2 => "switch_file"; (* TODO: merge with Ctrl+O? *)
  Ctrl_n => "new";
  Alt_letter_x => "execute_command";
  Alt_ctrl_x => "execute_process";

  (* Prompt Bindings *)
  let (=>) = Command.bind Prompt state in

  Ctrl_g => "cancel";
  Return => "validate";

  (* List choice Bindings *)
  let (=>) = Command.bind List_choice state in

  Ctrl_g => "cancel";
  Up => "choose_next";
  Down => "choose_previous";
  Return => "validate";

  (* Help Bindings *)
  let (=>) = Command.bind Help state in

  Ctrl_g => "cancel";
  Letter_q => "cancel";
  Return => "follow_link";

  List.iter (fun filename -> state.run_file filename state) init_filenames;

  Term_run.run_raw_mode
    ~on_key_press: (State.on_key_press state)
    ~on_render: (State.render state)
    ()

let () =
  Printexc.record_backtrace true;
  try
    main ()
  with
    | State.Exit ->
        ()
    | exn ->
        Printexc.print_backtrace stderr;
        prerr_endline (Printexc.to_string exn);
        exit 1
