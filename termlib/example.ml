(* Lines to display, in reverse order (newest first).
   Old lines are never deleted in this example. *)
let lines = ref []

let say ?(style = Render.default) x =
  Printf.ksprintf (fun s -> lines := (style, s) :: !lines) x

(* Demo of Utf8.split_runes. *)
let () =
  let u = "One: A, two: ç, three: ◸, invalid: \128!" in
  say "%S" u;
  u
  |> Utf8.split_runes
  |> List.map (Printf.sprintf "[%s]")
  |> String.concat ""
  |> say "%s"

let terminal_size = ref (80, 20)

let render () =
  let (w, h) = !terminal_size in
  let frame = Render.create_frame w h in

  (* Draw a border. *)
  let border_cell_h = Render.cell "=" in
  let border_cell_v = Render.cell "|" in

  for x = 0 to w - 1 do
    Render.set frame x 0 border_cell_h;
    Render.set frame x (h - 1) border_cell_h
  done;

  for y = 1 to h - 2 do
    Render.set frame 0 y border_cell_v;
    Render.set frame (w - 1) y border_cell_v
  done;

  (* Draw lines. *)
  let rec loop y lines =
    if y >= 1 then
      match lines with
        | [] ->
            ()
        | (style, line) :: tail ->
            Render.text_to_frame ~style frame 1 y (w - 2) line;
            loop (y - 1) tail
  in
  loop (h - 2) !lines;

  frame

let main () =
  Term.raw_mode ();
  Term.hide_cursor ();

  let input_state = ref Term.Empty in
  let previous_frame = ref None in

  while true do
    if Term.size_changed () then (
      Term.goto_bottom_right ();
      Term.query_cursor_position ();
      flush stdout;
    );

    let frame = render () in
    Render.output ?previous_frame: !previous_frame frame;
    previous_frame := Some frame;

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

      input_state := Term.input_char !input_state char;

      (
        match !input_state with
          | Invalid (state, char) ->
              input_state := Empty;
              say "Invalid input state: %s followed by %C"
                (Term.show_input_state state) char;

          | Done key ->
              input_state := Empty;

              (
                match Key.symbol key with
                  | ASCII char ->
                      say "You pressed: %c" char
                  | Unicode s ->
                      say
                        ~style: (Render.style ~intensity: Bold ())
                        "You pressed unicode: %s" s
                  | Control ->
                      say
                        ~style: (Render.style ~fg_color: Term.Red ())
                        "You pressed control character: %s"
                        (Key.show key)
              );

              (
                match key with
                  | Ctrl_c
                  | Ctrl_d ->
                      exit 0
                  | _ ->
                      ()
              );

          | Cursor_position (x, y) ->
              input_state := Empty;
              say "Terminal size is: %d × %d" x y;
              terminal_size := (x, y)

          | _ ->
              ()
      );
    done
  done

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
