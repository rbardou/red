type split_direction =
  | Vertical (* top and bottom *)
  | Horizontal (* left and right *)

type split_position =
  | Absolute_first of int
  | Absolute_second of int
  | Ratio of int * int

type t =
  | Single of Panel.t
  | Split of split_direction * split_position * bool * t * t (* draw split line, top or left, bottom or right *)

let single panel =
  Single panel

let vertical_split ?(pos = Ratio (1, 2)) ?(line = false) top bottom =
  Split (Vertical, pos, line, top, bottom)

let horizontal_split ?(pos = Ratio (1, 2)) ?(line = false) left right =
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

let rec get_top_left_panel layout =
  match layout with
    | Single panel ->
        panel
    | Split (_, _, _, sublayout, _) ->
        get_top_left_panel sublayout

type get_panel_result =
  | Panel_not_found
  | Found_panel
  | Found_result of Panel.t

let option_of_get_panel_result = function
  | Panel_not_found | Found_panel -> None
  | Found_result panel -> Some panel

let get_panel_right panel layout =
  let rec find layout =
    match layout with
      | Single single_panel ->
          if single_panel == panel then
            Found_panel
          else
            Panel_not_found
      | Split (Vertical, _, _, top, bottom) ->
          (
            match find top with
              | Panel_not_found ->
                  find bottom
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
      | Split (Horizontal, _, _, left, right) ->
          (
            match find left with
              | Panel_not_found ->
                  find right
              | Found_panel ->
                  Found_result (get_top_left_panel right)
              | Found_result _ as x ->
                  x
          )
  in
  option_of_get_panel_result (find layout)

let get_panel_left panel layout =
  let rec find layout =
    match layout with
      | Single single_panel ->
          if single_panel == panel then
            Found_panel
          else
            Panel_not_found
      | Split (Vertical, _, _, top, bottom) ->
          (
            match find top with
              | Panel_not_found ->
                  find bottom
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
      | Split (Horizontal, _, _, left, right) ->
          (
            match find right with
              | Panel_not_found ->
                  find left
              | Found_panel ->
                  Found_result (get_top_left_panel left)
              | Found_result _ as x ->
                  x
          )
  in
  option_of_get_panel_result (find layout)

let get_panel_down panel layout =
  let rec find layout =
    match layout with
      | Single single_panel ->
          if single_panel == panel then
            Found_panel
          else
            Panel_not_found
      | Split (Vertical, _, _, top, bottom) ->
          (
            match find top with
              | Panel_not_found ->
                  find bottom
              | Found_panel ->
                  Found_result (get_top_left_panel bottom)
              | Found_result _ as x ->
                  x
          )
      | Split (Horizontal, _, _, left, right) ->
          (
            match find right with
              | Panel_not_found ->
                  find left
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
  in
  option_of_get_panel_result (find layout)

let get_panel_up panel layout =
  let rec find layout =
    match layout with
      | Single single_panel ->
          if single_panel == panel then
            Found_panel
          else
            Panel_not_found
      | Split (Vertical, _, _, top, bottom) ->
          (
            match find bottom with
              | Panel_not_found ->
                  find top
              | Found_panel ->
                  Found_result (get_top_left_panel top)
              | Found_result _ as x ->
                  x
          )
      | Split (Horizontal, _, _, left, right) ->
          (
            match find right with
              | Panel_not_found ->
                  find left
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
  in
  option_of_get_panel_result (find layout)

let rec replace_panel panel replacement layout =
  match layout with
    | Single single_panel ->
        if single_panel == panel then
          Some replacement
        else
          None
    | Split (dir, pos, line, a, b) ->
        match replace_panel panel replacement a with
          | Some new_a ->
              Some (Split (dir, pos, line, new_a, b))
          | None ->
              match replace_panel panel replacement b with
                | Some new_b ->
                    Some (Split (dir, pos, line, a, new_b))
                | None ->
                    None

type remove_panel_result =
  | Panel_not_found
  | Panel_found_exactly
  | Panel_found_and_removed of t * Panel.t

let remove_panel panel layout =
  let rec find layout =
    match layout with
      | Single single_panel ->
          if single_panel == panel then
            Panel_found_exactly
          else
            Panel_not_found
      | Split (dir, pos, line, a, b) ->
          match find a with
            | Panel_found_exactly ->
                Panel_found_and_removed (b, get_top_left_panel b)
            | Panel_found_and_removed (new_a, next_panel) ->
                Panel_found_and_removed (Split (dir, pos, line, new_a, b), next_panel)
            | Panel_not_found ->
                match find b with
                  | Panel_found_exactly ->
                      Panel_found_and_removed (a, get_top_left_panel a)
                  | Panel_found_and_removed (new_b, next_panel) ->
                      Panel_found_and_removed (Split (dir, pos, line, a, new_b), next_panel)
                  | Panel_not_found ->
                      Panel_not_found
  in
  match find layout with
    | Panel_not_found | Panel_found_exactly ->
        None
    | Panel_found_and_removed (new_layout, next_panel) ->
        Some (new_layout, next_panel)
