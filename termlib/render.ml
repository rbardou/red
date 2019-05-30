type cell =
  {
    character: string;
    style: Style.t;
  }

let empty_cell =
  {
    character = " ";
    style = Style.default;
  }

let cell ?(style = Style.default) character =
  {
    character;
    style;
  }

type frame =
  {
    width: int;
    height: int;
    grid: cell array array;
  }

let create_frame width height =
  {
    width;
    height;
    grid = Array.init height (fun _ -> Array.init width (fun _ -> empty_cell));
  }

let set frame x y cell =
  if y >= 0 && x >= 0 && y < frame.height && x < frame.width then
    frame.grid.(y).(x) <- cell

let get frame x y =
  frame.grid.(y).(x)

let empty = create_frame 0 0

let width frame = frame.width
let height frame = frame.height

let output ?previous_frame frame =
  (* If size changed, just ignore previous frame. *)
  let previous_frame =
    match previous_frame with
      | None ->
          None
      | Some previous_frame ->
          if
            previous_frame.width <> frame.width ||
            previous_frame.height <> frame.height
          then
            None
          else
            Some previous_frame
  in

  (* Contains [true] if there is no need to set cursor position. *)
  let cursor_ok = ref false in
  let style = ref Style.default in

  let goto_xy x y =
    if not !cursor_ok then (
      Term.goto_xy (x + 1) (y + 1);
      cursor_ok := true
    )
  in

  goto_xy 0 0;
  Term.reset_style ();

  if previous_frame = None then (
    Term.clear_from_cursor ();
  );

  for y = 0 to frame.height - 1 do
    let row = frame.grid.(y) in

    for x = 0 to frame.width - 1 do
      let cell = row.(x) in

      (* Only output cell if it has changed. *)
      if
        match previous_frame with
          | None ->
              true
          | Some previous_frame ->
              let previous_cell = previous_frame.grid.(y).(x) in
              cell <> previous_cell
      then (
        goto_xy x y;

        (* Output the difference in style with the current style. *)
        let cell_style = cell.style in

        if cell_style.intensity <> !style.intensity then
          Term.intensity cell_style.intensity;

        if cell_style.underline <> !style.underline then
          Term.underline cell_style.underline;

        if cell_style.fg <> !style.fg then
          Term.fg_color cell_style.fg;

        if cell_style.bg <> !style.bg then
          Term.bg_color cell_style.bg;

        (* Style is now the style of the current cell, save it for next cell. *)
        style := cell_style;

        (* Output cell contents. *)
        print_string cell.character;
      )
      else
        cursor_ok := false
    done;

    (* Always reset cursor after a line, in case line is not full width
       or that terminal is resized. *)
    cursor_ok := false
  done;

  flush stdout

let text ?style frame x y width text =
  let rec loop i l =
    if i >= width then
      i
    else
      match l with
        | [] ->
            i
        | head :: tail ->
            let cell = cell ?style head in
            set frame (x + i) y cell;
            loop (i + 1) tail
  in

  let len = loop 0 (Utf8.split_runes text) in

  let empty_cell =
    match style with
      | None -> empty_cell
      | Some style -> { empty_cell with style }
  in
  for i = len to width - 1 do
    set frame (x + i) y empty_cell
  done

let textf ?style frame x y width format =
  Printf.ksprintf (text ?style frame x y width) format
