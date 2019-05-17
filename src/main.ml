let main () =
  let state =
    let panel_1 =
      let view =
        let file =
          if Array.length Sys.argv > 1 then
            File.create_loading Sys.argv.(1)
          else
            File.create Text.empty
        in
        File.create_view file
      in
      Panel.create view
    in
    let panel_2 =
      let view =
        let file =
          if Array.length Sys.argv > 2 then
            File.create_loading Sys.argv.(2)
          else
            File.create Text.empty
        in
        File.create_view file
      in
      Panel.create view
    in
    let layout = Layout.horizontal_split (Layout.single panel_1) (Layout.single panel_2) in
    State.create layout
  in

  let (=>) = Command.bind state in

  Ctrl_q => "quit";
  Right => "move_right";
  Left => "move_left";
  Down => "move_down";
  Up => "move_up";

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
