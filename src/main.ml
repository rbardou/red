let render w h frame =
  let output = Render.create_frame w h in
  Frame.render (Context.all output) frame;
  output

let main () =
  Term.raw_mode ~restore_at_exit: false ();
  Term.hide_cursor ();

  let input_state = ref Term.Empty in
  let previous_frame = ref None in
  let terminal_size = ref (80, 20) in
  let frame =
    if Array.length Sys.argv > 1 then
      ref (Frame.load !terminal_size Sys.argv.(1))
    else
      ref (Frame.start !terminal_size)
  in
  let continue = ref true in

  while !continue do
    if Term.size_changed () then (
      Term.goto_bottom_right ();
      Term.query_cursor_position ();
      flush stdout;
    );

    let output = let w, h = !terminal_size in render w h !frame in
    Render.output ?previous_frame: !previous_frame output;
    previous_frame := Some output;

    let buf = Bytes.create 128 in
    let len =
      try
        Unix.read Unix.stdin buf 0 (Bytes.length buf)
      with Unix.Unix_error (Unix.EINTR, _, _) ->
        (* We possibly received the SIGWINCH signal. *)
        0
    in

    for i = 0 to len - 1 do
      let char = buf.[i] in

      input_state := match Term.input_char !input_state char with
        | Invalid (state, char) ->
            Empty
        | Done Ctrl_q ->
            continue := false;
            Empty
        | Done key ->
            frame := Frame.key_press key !frame;
            Empty
        | Cursor_position (x, y) ->
            terminal_size := (x, y);
            frame := Frame.set_size (x, y) !frame;
            Empty
        | state ->
            state
    done
  done;

  Term.restore ();
  print_endline "Bye."

let () =
  Printexc.record_backtrace true;
  try
    main ()
  with exn ->
    Term.restore ();
    Term.goto_xy 0 0;
    Term.clear ();
    Printexc.print_backtrace stdout;
    print_endline (Printexc.to_string exn)
