let main () =
  let state =
    let file =
      if Array.length Sys.argv > 1 then
        File.create_loading Sys.argv.(1)
      else
        File.create Text.empty
    in
    let panel_1 =
      let view = File.create_view file in
      Panel.create view
    in
    let panel_2 =
      let view = File.create_view file in
      Panel.create view
    in
    let panel_3 =
      let view = File.create_view file in
      Panel.create view
    in
    let layout =
      Layout.vertical_split
        (Layout.horizontal_split ~line: true (Layout.single panel_1) (Layout.single panel_2))
        (Layout.single panel_3)
    in
    State.create layout
  in

  let (=>) = Command.bind state in

  Ctrl_q => "quit";
  Ctrl_s => "save";
  Alt_ctrl_s => "save_as";
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

  Ctrl_c => "copy";
  Ctrl_x => "cut";
  Ctrl_v => "paste";

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
