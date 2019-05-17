type split_direction =
  | Vertical (* top and bottom *)
  | Horizontal (* left and right *)

type split_position =
  | Absolute_first of int (* set the size of the top or left sublayout, and the other one takes the rest *)
  | Absolute_second of int (* set the size of the bottom or right sublayout, and the other one takes the rest *)
  | Ratio of int * int (* numerator, denominator: denotes a fraction of the parent's size *)

type t =
  | Single of Panel.t
  | Split of split_direction * split_position * bool * t * t (* draw split line, top or left, bottom or right *)

let single panel =
  Single panel

let vertical_split ?(pos = Ratio (1, 2)) ?(line = true) left right =
  Split (Vertical, pos, line, left, right)

let horizontal_split ?(pos = Ratio (1, 2)) ?(line = true) left right =
  Split (Horizontal, pos, line, left, right)

let line_style =
  Render.style ~bg_color: White ~fg_color: Black ()

let horizontal_line_cell =
  Render.cell ~style: line_style "-"

let vertical_line_cell =
  Render.cell ~style: line_style "|"

let render_horizontal_line frame y x w =
  for i = x to x + w - 1 do
    Render.set frame i y horizontal_line_cell
  done

let render_vertical_line frame x y h =
  for i = y to y + h - 1 do
    Render.set frame x i vertical_line_cell
  done

let rec render render_panel frame ?(x = 0) ?(y = 0) ~w ~h layout: unit =
  match layout with
    | Single panel ->
        render_panel frame panel ~x ~y ~w ~h

    | Split (Vertical, Absolute_first size, false, top, bottom) ->
        render render_panel frame ~x ~y ~w ~h: size top;
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) bottom
    | Split (Vertical, Absolute_second size, false, top, bottom) ->
        render render_panel frame ~x ~y ~w ~h: (h - size) top;
        render render_panel frame ~x ~y: (y + h - size) ~w ~h: size bottom
    | Split (Vertical, Ratio (numerator, denumerator), false, top, bottom) ->
        let size = h * numerator / denumerator in
        render render_panel frame ~x ~y ~w ~h: size top;
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) bottom

    | Split (Horizontal, Absolute_first size, false, left, right) ->
        render render_panel frame ~x ~y ~w: size ~h left;
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h right
    | Split (Horizontal, Absolute_second size, false, left, right) ->
        render render_panel frame ~x ~y ~w: (w - size) ~h left;
        render render_panel frame ~x: (x + w - size) ~y ~w: size ~h right
    | Split (Horizontal, Ratio (numerator, denumerator), false, left, right) ->
        let size = w * numerator / denumerator in
        render render_panel frame ~x ~y ~w: size ~h left;
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h right

    | Split (Vertical, Absolute_first size, true, top, bottom) ->
        render render_panel frame ~x ~y ~w ~h: size top;
        render_horizontal_line frame (y + size) x w;
        let size = size + 1 in
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) bottom
    | Split (Vertical, Absolute_second size, true, top, bottom) ->
        render render_panel frame ~x ~y ~w ~h: (h - size - 1) top;
        render_horizontal_line frame (y + h - size - 1) x w;
        render render_panel frame ~x ~y: (y + h - size) ~w ~h: size bottom
    | Split (Vertical, Ratio (numerator, denumerator), true, top, bottom) ->
        let size = h * numerator / denumerator in
        render render_panel frame ~x ~y ~w ~h: size top;
        render_horizontal_line frame (y + size) x w;
        let size = size + 1 in
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) bottom

    | Split (Horizontal, Absolute_first size, true, left, right) ->
        render render_panel frame ~x ~y ~w: size ~h left;
        render_vertical_line frame (x + size) y h;
        let size = size + 1 in
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h right
    | Split (Horizontal, Absolute_second size, true, left, right) ->
        render render_panel frame ~x ~y ~w: (w - size) ~h left;
        render_vertical_line frame (x + w - size - 1) y h;
        render render_panel frame ~x: (x + w - size) ~y ~w: size ~h right
    | Split (Horizontal, Ratio (numerator, denumerator), true, left, right) ->
        let size = w * numerator / denumerator in
        render render_panel frame ~x ~y ~w: size ~h left;
        render_vertical_line frame (x + size) y h;
        let size = size + 1 in
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h right

let rec get_first_panel layout =
  match layout with
    | Single panel ->
        panel
    | Split (_, _, _, sublayout, _) ->
        get_first_panel sublayout
