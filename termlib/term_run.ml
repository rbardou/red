let rec spawn_input_reader
    ~on_key_press
    ?(on_cursor_position = fun _ _ -> ())
    ?(state = Term.Empty)
    () =
  Spawn.on_read Unix.stdin @@ fun () ->

  let buf = Bytes.create 1024 in
  let len =
    try
      Unix.read Unix.stdin buf 0 (Bytes.length buf)
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      (* We possibly received the SIGWINCH signal. *)
      0
  in

  let rec loop state i =
    if i >= len then
      spawn_input_reader
        ~on_key_press
        ~on_cursor_position
        ~state
        ()
    else
      match Term.input_char state buf.[i] with
        | Invalid _ ->
            loop Empty (i + 1)
        | Done key ->
            on_key_press key;
            loop Empty (i + 1)
        | Cursor_position (x, y) ->
            on_cursor_position x y;
            loop Empty (i + 1)
        | state ->
            loop state (i + 1)
  in
  loop state 0

let run_raw_mode
    ~on_key_press
    ?(on_render = fun _ -> ())
    () =
  Term.hide_cursor ();
  Term.with_raw_mode @@ fun () ->

  let on_size_changed () =
    Term.goto_bottom_right ();
    Term.query_cursor_position ();
    flush stdout
  in

  let width = ref 80 in
  let height = ref 40 in
  let on_resize w h =
    width := w;
    height := h
  in

  spawn_input_reader
    ~on_key_press
    ~on_cursor_position: on_resize
    ();

  let previous_frame = ref None in
  let on_iterate () =
    if Term.size_changed () then on_size_changed ();
    (* TODO: if we already rendered something less than, say, 1ms ago, do not render again;
       instead, and only there isn't one already, spawn a thread which will render a little later. *)
    let w = !width in
    let h = !height in
    let new_frame = Render.create_frame w h in
    on_render new_frame;
    Render.output ?previous_frame: !previous_frame new_frame;
    previous_frame := Some new_frame
  in

  Spawn.run ~on_iterate ()
