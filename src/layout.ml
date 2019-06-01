type split_direction =
  | Vertical (* top and bottom *)
  | Horizontal (* left and right *)

type split_position =
  | Absolute_first of int
  | Absolute_second of int
  | Ratio of int * int

type split_main =
  | First
  | Second

type t =
  | Single of Panel.t
  | Split of split

and split =
  {
    direction: split_direction;
    position: split_position;
    separator: bool;
    first: t;
    second: t;
    main: split_main;
  }

let single panel =
  Single panel

let create_file file =
  Single (Panel.create_file file)

let split direction ?(pos = Ratio (1, 2)) ?(sep = false) ?(main = First) first second =
  Split {
    direction;
    position = pos;
    separator = sep;
    first;
    second;
    main;
  }

let line_style =
  Style.make ~bg: White ~fg: Black ()

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

    | Split { direction = Vertical; position = Absolute_first size; separator = false; first; second } ->
        render render_panel frame ~x ~y ~w ~h: size first;
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) second
    | Split { direction = Vertical; position = Absolute_second size; separator = false; first; second } ->
        render render_panel frame ~x ~y ~w ~h: (h - size) first;
        render render_panel frame ~x ~y: (y + h - size) ~w ~h: size second
    | Split { direction = Vertical; position = Ratio (numerator, denumerator); separator = false; first; second } ->
        let size = h * numerator / denumerator in
        render render_panel frame ~x ~y ~w ~h: size first;
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) second

    | Split { direction = Horizontal; position = Absolute_first size; separator = false; first; second } ->
        render render_panel frame ~x ~y ~w: size ~h first;
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h second
    | Split { direction = Horizontal; position = Absolute_second size; separator = false; first; second } ->
        render render_panel frame ~x ~y ~w: (w - size) ~h first;
        render render_panel frame ~x: (x + w - size) ~y ~w: size ~h second
    | Split { direction = Horizontal; position = Ratio (numerator, denumerator); separator = false; first; second } ->
        let size = w * numerator / denumerator in
        render render_panel frame ~x ~y ~w: size ~h first;
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h second

    | Split { direction = Vertical; position = Absolute_first size; separator = true; first; second } ->
        render render_panel frame ~x ~y ~w ~h: size first;
        render_horizontal_line frame (y + size) x w;
        let size = size + 1 in
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) second
    | Split { direction = Vertical; position = Absolute_second size; separator = true; first; second } ->
        render render_panel frame ~x ~y ~w ~h: (h - size - 1) first;
        render_horizontal_line frame (y + h - size - 1) x w;
        render render_panel frame ~x ~y: (y + h - size) ~w ~h: size second
    | Split { direction = Vertical; position = Ratio (numerator, denumerator); separator = true; first; second } ->
        let size = h * numerator / denumerator in
        render render_panel frame ~x ~y ~w ~h: size first;
        render_horizontal_line frame (y + size) x w;
        let size = size + 1 in
        render render_panel frame ~x ~y: (y + size) ~w ~h: (h - size) second

    | Split { direction = Horizontal; position = Absolute_first size; separator = true; first; second } ->
        render render_panel frame ~x ~y ~w: size ~h first;
        render_vertical_line frame (x + size) y h;
        let size = size + 1 in
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h second
    | Split { direction = Horizontal; position = Absolute_second size; separator = true; first; second } ->
        render render_panel frame ~x ~y ~w: (w - size) ~h first;
        render_vertical_line frame (x + w - size - 1) y h;
        render render_panel frame ~x: (x + w - size) ~y ~w: size ~h second
    | Split { direction = Horizontal; position = Ratio (numerator, denumerator); separator = true; first; second } ->
        let size = w * numerator / denumerator in
        render render_panel frame ~x ~y ~w: size ~h first;
        render_vertical_line frame (x + size) y h;
        let size = size + 1 in
        render render_panel frame ~x: (x + size) ~y ~w: (w - size) ~h second

let rec get_main_panel layout =
  match layout with
    | Single panel ->
        panel
    | Split ({ main = First; first = main } | { main = Second; second = main }) ->
        get_main_panel main

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
      | Split { direction = Vertical; first; second } ->
          (
            match find first with
              | Panel_not_found ->
                  find second
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
      | Split { direction = Horizontal; first; second } ->
          (
            match find first with
              | Panel_not_found ->
                  find second
              | Found_panel ->
                  Found_result (get_main_panel second)
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
      | Split { direction = Vertical; first; second } ->
          (
            match find first with
              | Panel_not_found ->
                  find second
              | Found_panel ->
                  Found_panel
              | Found_result _ as x ->
                  x
          )
      | Split { direction = Horizontal; first; second } ->
          (
            match find second with
              | Panel_not_found ->
                  find first
              | Found_panel ->
                  Found_result (get_main_panel first)
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
      | Split { direction = Vertical; first; second } ->
          (
            match find first with
              | Panel_not_found ->
                  find second
              | Found_panel ->
                  Found_result (get_main_panel second)
              | Found_result _ as x ->
                  x
          )
      | Split { direction = Horizontal; first; second } ->
          (
            match find second with
              | Panel_not_found ->
                  find first
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
      | Split { direction = Vertical; first; second } ->
          (
            match find second with
              | Panel_not_found ->
                  find first
              | Found_panel ->
                  Found_result (get_main_panel first)
              | Found_result _ as x ->
                  x
          )
      | Split { direction = Horizontal; first; second } ->
          (
            match find second with
              | Panel_not_found ->
                  find first
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
    | Split split ->
        match replace_panel panel replacement split.first with
          | Some first ->
              Some (Split { split with first })
          | None ->
              match replace_panel panel replacement split.second with
                | Some second ->
                    Some (Split { split with second })
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
      | Split split ->
          match find split.first with
            | Panel_found_exactly ->
                Panel_found_and_removed (split.second, get_main_panel split.second)
            | Panel_found_and_removed (first, next_panel) ->
                Panel_found_and_removed (Split { split with first }, next_panel)
            | Panel_not_found ->
                match find split.second with
                  | Panel_found_exactly ->
                      Panel_found_and_removed (split.first, get_main_panel split.first)
                  | Panel_found_and_removed (second, next_panel) ->
                      Panel_found_and_removed (Split { split with second }, next_panel)
                  | Panel_not_found ->
                      Panel_not_found
  in
  match find layout with
    | Panel_not_found | Panel_found_exactly ->
        None
    | Panel_found_and_removed (new_layout, next_panel) ->
        Some (new_layout, next_panel)

let rec panel_is_visible panel layout =
  match layout with
    | Single single_panel ->
        single_panel == panel
    | Split { first; second } ->
        panel_is_visible panel first || panel_is_visible panel second

let rec foreach_panel layout f =
  match layout with
    | Single panel ->
        f panel
    | Split { first; second } ->
        foreach_panel first f;
        foreach_panel second f
