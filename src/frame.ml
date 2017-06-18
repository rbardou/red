(* Marks are indexes in a map to their position. *)
type mark = int

type cursor =
  {
    mark: mark;
    selection_start: mark;
    clipboard: Text.t;
  }

module Int =
struct
  type t = int
  let compare = (Pervasives.compare: int -> int -> int)
end

module Int_map = Map.Make (Int)

type t =
  {
    width: int;
    height: int;
    scroll_x: int;
    scroll_y: int;
    cursors: cursor Sequence.t;
    text: Text.t;
    marks: (int * int) Int_map.t;
    next_mark: mark;
    name: string;
    modified: bool;
    filename: string option;
  }

let start (width, height) =
  {
    width;
    height;
    scroll_x = 0;
    scroll_y = 0;
    cursors =
      Sequence.one {
        mark = 0;
        selection_start = 1;
        clipboard = Text.empty;
      };
    text = Text.empty;
    marks = Int_map.add 1 (0, 0) (Int_map.singleton 0 (0, 0));
    next_mark = 2;
    name = "(unnamed)";
    modified = false;
    filename = None;
  }

let load size filename =
  if System.file_exists filename then
    {
      (start size) with
        text = Text.load_file filename;
        name = Filename.basename filename;
        filename = Some filename;
    }
  else
    {
      (start size) with
        name = Filename.basename filename;
        filename = Some filename;
        modified = true;
    }

let fresh_mark (frame: t): mark * t =
  frame.next_mark, { frame with next_mark = frame.next_mark + 1 }

let set_size (width, height) (frame: t): t =
  { frame with width; height }

let line_count (frame: t) =
  Text.line_count frame.text

let line_length y (frame: t) =
  Text.line_length y frame.text

let text_width (frame: t) = frame.width
let text_height (frame: t) = frame.height - 1 (* scrollbar has height 1 *)

let get_mark (mark: mark) (frame: t) =
  match Int_map.find mark frame.marks with
    | exception Not_found ->
        (* TODO: This should not happen, log a warning somewhere. *)
        0, 0
    | xy ->
        xy

let set_mark (mark: mark) xy (frame: t): t =
  let marks = Int_map.add mark xy frame.marks in
  { frame with marks }

let remove_mark (mark: mark) xy (frame: t): t =
  let marks = Int_map.remove mark frame.marks in
  { frame with marks }

let get_cursor_position (cursor: cursor) (frame: t) =
  get_mark cursor.mark frame

let set_cursor_position (cursor: cursor) xy (frame: t): t =
  set_mark cursor.mark xy frame

let fix_cursor_position (cursor: cursor) (frame: t): t =
  let x, y = get_cursor_position cursor frame in
  let len = line_length y frame in
  if x > len then
    set_cursor_position cursor (len, y) frame
  else
    frame

let selection_is_empty (cursor: cursor) (frame: t): bool =
  let cx, cy = get_mark cursor.mark frame in
  let sx, sy = get_mark cursor.selection_start frame in
  cx = sx && cy = sy

type line_range =
  {
    (* First character included in the range. *)
    first: int;
    (* Last character included in the range.
       If [None], range ends at newline, which is included. *)
    last: int option;
  }

(* Return position of:
   - the first character of a cursor's selection;
   - the last of a cursor's selection.

   The last character is not actually included in the selection.
   But it can be on the next line to end the selection with a newline. *)
let selection_range (cursor: cursor) (frame: t): (int * int) * (int * int) =
  let cx, cy = get_cursor_position cursor frame in
  let sx, sy = get_mark cursor.selection_start frame in
  if cy < sy || cy = sy && cx < sx then
    (cx, cy), (sx, sy)
  else
    (sx, sy), (cx, cy)

(* Represent a selection as a list of line ranges, in reverse order. *)
let ranges_of_selection (cursor: cursor) (frame: t):
  (int * line_range) list =
  let (sx, sy), (cx, cy) = selection_range cursor frame in

  if cy > sy then (
    let result = ref [] in

    (* First line misses characters at the left. *)
    result := (sy, { first = sx; last = None }) :: !result;

    (* Intermediate lines are complete. *)
    for y = sy + 1 to cy - 1 do
      result := (y, { first = 0; last = None }) :: !result;
    done;

    (* Last line misses characters at the right. *)
    result := (cy, { first = 0; last = Some (cx - 1) }) :: !result;

    !result
  ) else
    (* Selection ranges over only one line, incomplete from both ends. *)
    [ cy, { first = sx; last = Some (cx - 1) } ]

let move_left (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  let xy =
    if cx <= 0 then
      if cy <= 0 then
        0, 0
      else
        let cy = cy - 1 in
        line_length cy frame, cy
    else
      cx - 1, cy
  in
  set_cursor_position cursor xy frame

let move_right (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  let xy =
    let len = line_length cy frame in
    if cx >= len then
      let count = line_count frame in
      if cy >= count - 1 then
        len, count - 1
      else
        0, cy + 1
    else
      cx + 1, cy
  in
  set_cursor_position cursor xy frame

let move_up (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  let xy =
    if cy <= 0 then
      cx, cy
    else
      (* TODO: end of line *)
      cx, cy - 1
  in
  set_cursor_position cursor xy frame

let move_down (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  let count = line_count frame in
  let xy =
    if cy >= count - 1 then
      cx, count - 1
    else
      (* TODO: end of line *)
      cx, cy + 1
  in
  set_cursor_position cursor xy frame

let delete_line_range (frame: t) (y, range): t =
  match range with
    | { first = x; last = None } ->
        let text = Text.split_line (x, y) frame.text in
        let text = Text.remove_line (y + 1) text in
        let text = Text.merge_lines y text in
        let marks =
          let shift (mx, my) =
            if my = y then
              if mx <= x then
                mx, y
              else
                x, y
            else if my = y + 1 then
              mx + x, y
            else if my > y then
              mx, my - 1
            else
              mx, my
          in
          Int_map.map shift frame.marks
        in
        { frame with text; marks; modified = true }

    | { first = x1; last = Some x2 } ->
        let text = Text.split_line (x1, y) frame.text in
        let text = Text.split_line (x2 - x1 + 1, y + 1) text in
        let text = Text.remove_line (y + 1) text in
        let text = Text.merge_lines y text in
        let marks =
          let shift (mx, my) =
            if my = y then
              if mx >= x1 then
                if mx > x2 then
                  mx - x2 + x1 - 1, my
                else
                  x1, my
              else
                mx, my
            else
              mx, my
          in
          Int_map.map shift frame.marks
        in
        { frame with text; marks; modified = true }

let delete_selection (cursor: cursor) (frame: t): t =
  (* Ranges are in reverse order, we can remove lines without messing with
     their indexes. *)
  List.fold_left delete_line_range frame (ranges_of_selection cursor frame)

let insert (character: Text.character) (cursor: cursor) (frame: t): t =
  let frame = fix_cursor_position cursor frame in
  let frame = delete_selection cursor frame in

  let cx, cy = get_cursor_position cursor frame in
  let text = Text.insert (cx, cy) character frame.text in

  (* Shift all marks after [cursor] on the same line, including [cursor]. *)
  let marks =
    let shift (x, y) = if y = cy && x >= cx then x + 1, y else x, y in
    Int_map.map shift frame.marks
  in

  { frame with text; marks; modified = true }

let split_line (cursor: cursor) (frame: t): t =
  let frame = fix_cursor_position cursor frame in
  let frame = delete_selection cursor frame in

  let cx, cy = get_cursor_position cursor frame in
  let text = Text.split_line (cx, cy) frame.text in

  (* Shift all marks after [cursor], including [cursor]. *)
  let marks =
    let shift (x, y) =
      if y = cy && x >= cx then
        x - cx, y + 1
      else if y > cy then
        x, y + 1
      else
        x, y
    in
    Int_map.map shift frame.marks
  in

  { frame with text; marks; modified = true }

let delete (cursor: cursor) (frame: t): t =
  let frame = fix_cursor_position cursor frame in

  if selection_is_empty cursor frame then (
    let cx, cy = get_cursor_position cursor frame in
    let len = line_length cy frame in

    if cx >= len then
      let text = Text.merge_lines cy frame.text in

      (* Shift marks after [cursor], i.e. at lines after [cy]. *)
      let marks =
        let shift (x, y) =
          if y = cy + 1 then
            x + len, y - 1
          else if y > cy then
            x, y - 1
          else
            x, y
        in
        Int_map.map shift frame.marks
      in

      { frame with text; marks; modified = true }

    else
      let text = Text.remove (cx, cy) frame.text in

      (* Shift marks after [cursor] on the same line, not including [cursor]. *)
      let marks =
        let shift (x, y) = if y = cy && x > cx then x - 1, y else x, y in
        Int_map.map shift frame.marks
      in

      { frame with text; marks; modified = true }
  )
  else
    delete_selection cursor frame

let delete_backward (cursor: cursor) (frame: t): t =
  let frame = fix_cursor_position cursor frame in

  if selection_is_empty cursor frame then (
    let cx, cy = get_cursor_position cursor frame in
    if cx = 0 && cy = 0 then
      frame
    else
      frame
      |> move_left cursor
      |> delete cursor
  )
  else
    delete_selection cursor frame

let move_to_beginning_of_line (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  set_cursor_position cursor (0, cy) frame

let move_to_end_of_line (cursor: cursor) (frame: t): t =
  let cx, cy = get_cursor_position cursor frame in
  set_cursor_position cursor (line_length cy frame, cy) frame

let recenter_right (cursor: cursor) (frame: t): t =
  let x, _ = get_cursor_position cursor frame in
  { frame with scroll_x = max 0 (x - frame.width * 3 / 4) }

let recenter_left (cursor: cursor) (frame: t): t =
  let x, _ = get_cursor_position cursor frame in
  { frame with scroll_x = max 0 (x - frame.width * 1 / 4) }

let recenter_down (cursor: cursor) (frame: t): t =
  let _, y = get_cursor_position cursor frame in
  { frame with scroll_y = max 0 (y - frame.height * 3 / 4) }

let recenter_up (cursor: cursor) (frame: t): t =
  let _, y = get_cursor_position cursor frame in
  { frame with scroll_y = max 0 (y - frame.height * 1 / 4) }

let recenter_if_out (frame: t) =
  match Sequence.get 0 frame.cursors with
    | None ->
        frame
    | Some cursor ->
        let x, y = get_cursor_position cursor frame in
        let frame =
          if x < frame.scroll_x then
            recenter_right cursor frame
          else if x >= frame.scroll_x + text_width frame then
            recenter_left cursor frame
          else
            frame
        in
        if y < frame.scroll_y then
          recenter_down cursor frame
        else if y >= frame.scroll_y + text_height frame then
          recenter_up cursor frame
        else
          frame

let recenter_vertically (frame: t) =
  match Sequence.get 0 frame.cursors with
    | None ->
        frame
    | Some cursor ->
        let _, y = get_cursor_position cursor frame in
        if y < frame.scroll_y + text_height frame / 2 then
          recenter_down cursor frame
        else
          recenter_up cursor frame

let reset_selection (cursor: cursor) (frame: t): t =
  set_mark cursor.selection_start (get_cursor_position cursor frame) frame

let for_all_cursors (f: cursor -> t -> t) (frame: t): t =
  let frame = ref frame in
  for i = 0 to Sequence.count !frame.cursors - 1 do
    match Sequence.get i !frame.cursors with
      | None ->
          (* Cursor was deleted, we can stop here. *)
          ()
      | Some cursor ->
          frame := f cursor !frame
  done;
  !frame

let scroll (x, y) (frame: t): t =
  let x = max x (- frame.scroll_x) in
  let y =
    min
      (max y (- frame.scroll_y))
      (line_count frame - frame.scroll_y - 2)
  in
  let frame =
    let shift frame cursor =
      if selection_is_empty cursor frame then
        let cx, cy = get_cursor_position cursor frame in
        set_cursor_position cursor (cx + x, cy + y) frame
        |> reset_selection cursor
      else
        frame
    in
    Sequence.fold shift frame frame.cursors
  in
  { frame with scroll_x = frame.scroll_x + x; scroll_y = frame.scroll_y + y }

let scroll_left n (frame: t): t = scroll (n, 0) frame
let scroll_right n (frame: t): t = scroll (-n, 0) frame
let scroll_up n (frame: t): t = scroll (0, n) frame
let scroll_down n (frame: t): t = scroll (0, -n) frame

let multiply_cursors (frame: t): t =
  match Sequence.get 0 frame.cursors with
    | None ->
        frame
    | Some main_cursor ->
        if Sequence.count frame.cursors > 1 then
          { frame with cursors = Sequence.one main_cursor }
        else
          let cx, cy = get_cursor_position main_cursor frame in
          let sx, sy = get_mark main_cursor.selection_start frame in
          let x = cx in
          let y1, y2 = if cy < sy then cy, sy else sy, cy in

          let cursors = ref Sequence.empty in
          let frame = ref frame in

          (* Remove existing cursors. *)
          let remove_cursor (cursor: cursor) =
            let marks = Int_map.remove cursor.selection_start !frame.marks in
            let marks = Int_map.remove cursor.mark marks in
            frame := { !frame with marks }
          in
          Sequence.iter remove_cursor !frame.cursors;

          (* Create new cursors. *)
          for y = y1 to y2 do
            let selection_start, new_frame = fresh_mark !frame in
            let mark, new_frame = fresh_mark new_frame in
            let cursor =
              {
                selection_start;
                mark;
                clipboard = main_cursor.clipboard;
              }
            in
            cursors := Sequence.append cursor !cursors;
            let marks = new_frame.marks in
            let marks = Int_map.add selection_start (x, y) marks in
            let marks = Int_map.add mark (x, y) marks in
            frame := { new_frame with cursors = !cursors; marks }
          done;

          !frame

let save (frame: t): t =
  match frame.filename with
    | None ->
        (* TODO: Prompt for filename. *)
        frame
    | Some filename ->
        Text.save_file filename frame.text;
        { frame with modified = false }

let copy (frame: t): t =
  let copy_cursor (cursor: cursor): cursor =
    let sxy, (ex, ey) = selection_range cursor frame in
    let exy =
      (* x = -1 will cause the newline to be included but not the first
         character of the line. *)
      ex - 1, ey
    in
    let clipboard = Text.sub sxy exy frame.text in
    { cursor with clipboard }
  in
  let cursors = Sequence.map copy_cursor frame.cursors in
  { frame with cursors }

let paste_cursor (cursor: cursor) (frame: t): t =
  let frame = fix_cursor_position cursor frame in
  let frame = delete_selection cursor frame in

  let (cx, cy) as cxy = get_cursor_position cursor frame in
  let clipboard = cursor.clipboard in

  let text = Text.insert_sub cxy clipboard frame.text in

  (* Shift marks at or after [cursor].*)
  let marks =
    let clipboard_line_count = max 1 (Text.line_count clipboard) in
    let shift (x, y) =
      if y < cy || y = cy && x < cx then
        x, y
      else if y = cy then
        let clipboard_last_line_length =
          Text.line_length (clipboard_line_count - 1) clipboard
        in
        let x =
          if clipboard_line_count = 1 then
            x + clipboard_last_line_length
          else
            x + clipboard_last_line_length - cx
        in
        let y = y + clipboard_line_count - 1 in
        x, y
      else
        x, y + clipboard_line_count - 1
    in
    Int_map.map shift frame.marks
  in

  { frame with text; marks; modified = true }

let paste = for_all_cursors paste_cursor

let (>>) (f: t -> t) (g: t -> t) (frame: t): t =
  g (f frame)

let (>>>) (f: cursor -> t -> t) (g: cursor -> t -> t)
    (cursor: cursor) (frame: t): t =
  g cursor (f cursor frame)

let cut = copy >> for_all_cursors delete_selection

let key_press (key: Key.t): t -> t =
  let movement move =
    for_all_cursors (move >>> reset_selection) >> recenter_if_out
  in
  let selection move = for_all_cursors move >> recenter_if_out in
  match key with
    | F5 -> recenter_vertically

    | Left -> movement move_left
    | Right -> movement move_right
    | Up -> movement move_up
    | Down -> movement move_down
    | Home -> movement move_to_beginning_of_line
    | End -> movement move_to_end_of_line

    | Shift_left -> selection move_left
    | Shift_right -> selection move_right
    | Shift_up -> selection move_up
    | Shift_down -> selection move_down
    | Shift_home -> selection move_to_beginning_of_line
    | Shift_end -> selection move_to_end_of_line

    | Return -> for_all_cursors split_line >> recenter_if_out
    | Delete -> for_all_cursors delete >> recenter_if_out
    | Backspace -> for_all_cursors delete_backward >> recenter_if_out

    | Alt_right -> scroll_left 1
    | Alt_left -> scroll_right 1
    | Alt_down -> scroll_up 1
    | Alt_up -> scroll_down 1
    | Alt_double_quote -> multiply_cursors

    | Page_down -> (fun frame -> scroll_up (text_height frame * 3 / 4) frame)
    | Page_up -> (fun frame -> scroll_down (text_height frame * 3 / 4) frame)

    | Ctrl_s -> save

    | Ctrl_c -> copy
    | Ctrl_x -> cut
    | Ctrl_v -> paste

    | _ ->
        match Key.symbol key with
          | ASCII c -> for_all_cursors (insert (String.make 1 c))
          | Unicode s -> for_all_cursors (insert s)
          | Control -> (fun frame -> frame)

let change_character_style (ctx: Context.t) (frame: t)
    (f: Render.style -> Render.style) (x, y) =
  let x = x - frame.scroll_x and y = y - frame.scroll_y in
  let cell = Context.get ctx x y in
  let style = f cell.style in
  Context.set ctx x y { cell with style }

let render_selection (ctx: Context.t) (frame: t) (cursor: cursor) =
  let width = ctx.width in

  let set_style =
    change_character_style ctx frame @@ fun style ->
    { style with bg_color = Yellow }
  in

  (* Iterate over all positions contained within a selection,
     from top-left to bottom-right. *)
  let iter_selection (f: int * int -> unit) (cursor: cursor) (frame: t) =
    let ranges = ranges_of_selection cursor frame in

    let iter_range (y, range) =
      let last =
        match range.last with
          | None -> width - 1
          | Some x -> x
      in
      for x = range.first to last do
        f (x, y)
      done
    in

    List.iter iter_range ranges
  in

  iter_selection set_style cursor frame

let render_cursor (ctx: Context.t) (frame: t) (cursor: cursor) =
  change_character_style ctx frame
    (fun style -> { style with fg_color = Black; bg_color = White })
    (get_cursor_position cursor frame)

let make_status (frame: t): string =
  let cursors =
    let count = Sequence.count frame.cursors in
    if count > 1 then
      Printf.sprintf "(%d cursors)" count
    else
      match Sequence.get 0 frame.cursors with
        | None ->
            "(no cursor)"
        | Some cursor ->
            let x, y = get_cursor_position cursor frame in
            Printf.sprintf "(%d, %d)" x y
  in
  let modified =
    if frame.modified then " (modified)" else ""
  in
  Printf.sprintf "%s %s%s" frame.name cursors modified

let render (ctx: Context.t) (frame: t) =
  (* Text area. *)
  for y = 0 to ctx.height - 2 do
    for x = 0 to ctx.width - 1 do
      let text_xy = x + frame.scroll_x, y + frame.scroll_y in
      let character = Text.get text_xy frame.text in
      let character =
        if character = "\t" then
          " "
        else if String.length character = 1 && character.[0] < '\032' then
          "?"
        else
          character
      in
      Context.set ctx x y (Render.cell character)
    done;
  done;

  (* Selections. *)
  Sequence.iter (render_selection ctx frame) frame.cursors;

  (* Cursors. *)
  Sequence.iter (render_cursor ctx frame) frame.cursors;

  (* Status bar. *)
  Context.render_text ctx 0 (ctx.height - 1) (make_status frame)
    ~style: (Render.style ~fg_color: Black ~bg_color: White ())
