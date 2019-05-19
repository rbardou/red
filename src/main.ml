let main () =
  (* Load files given on the command line. *)
  let files =
    match Array.to_list Sys.argv with
      | [] | [ _ ] ->
          [ File.create Text.empty ]
      | _ :: filenames ->
          let load_file filename =
            let file = File.create Text.empty in
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
  let state = State.create layout in
  state.files <- files;

  (* Global Bindings *)
  let (=>) = Command.bind Global state in

  Alt_escape => "quit";
  F4 => "remove_panel"; (* TODO: better binding? *)

  Right => "move_right";
  Left => "move_left";
  Down => "move_down";
  Up => "move_up";
  End => "move_end_of_line";
  Home => "move_beginning_of_line";
  Ctrl_end => "move_end_of_file";
  Ctrl_home => "move_beginning_of_file";

  Ctrl_a => "select_all";
  Shift_right => "select_right";
  Shift_left => "select_left";
  Shift_down => "select_down";
  Shift_up => "select_up";
  Shift_end => "select_end_of_line";
  Shift_home => "select_beginning_of_line";
  Ctrl_shift_end => "select_end_of_file";
  Ctrl_shift_home => "select_beginning_of_file";

  Alt_right => "focus_right";
  Alt_left => "focus_left";
  Alt_down => "focus_down";
  Alt_up => "focus_up";

  Page_down => "scroll_down";
  Page_up => "scroll_up";

  Return => "insert_new_line";
  Delete => "delete_character";
  Backspace => "delete_character_backwards";
  Alt_double_quote => "create_cursors_from_selection";
  Ctrl_k => "delete_end_of_line";

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
  Ctrl_n => "new";

  (* Prompt Bindings *)
  let (=>) = Command.bind Prompt state in

  Return => "validate";

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
