(** Panel layouts. *)

(** Where to split a layout. *)
type split_position =
  | Absolute_first of int (** set the size of the top or left sublayout, and the other one takes the rest *)
  | Absolute_second of int (** set the size of the bottom or right sublayout, and the other one takes the rest *)
  | Ratio of int * int (** numerator, denominator: denotes a fraction of the parent's size *)

(** Panel layouts. *)
type t

(** A layout with a single panel. *)
val single: Panel.t -> t

(** A layout split between top and bottom. *)
val vertical_split: ?pos: split_position -> ?line: bool -> t -> t -> t

(** A layout split between left and right. *)
val horizontal_split: ?pos: split_position -> ?line: bool -> t -> t -> t

(** Get the top-left panel of a layout. *)
val get_top_left_panel: t -> Panel.t

(** Render a layout by computing panel positions and calling a render function on them. *)
val render:
  (Render.frame -> Panel.t -> x: int -> y: int -> w: int -> h: int -> unit) ->
  Render.frame -> ?x: int -> ?y: int -> w: int -> h: int -> t -> unit

(** Get the panel which is at the right of given panel.

    If several panels are at the right, return the top-left one. *)
val get_panel_right: Panel.t -> t -> Panel.t option

(** Get the panel which is at the left of given panel. *)
val get_panel_left: Panel.t -> t -> Panel.t option

(** Get the panel which is below a given panel. *)
val get_panel_down: Panel.t -> t -> Panel.t option

(** Get the panel which is above a given panel. *)
val get_panel_up: Panel.t -> t -> Panel.t option

(** Replace a panel by a layout.

    Usage: [replace_panel panel replacement layout]

    Return [None] if [panel] was not found in [layout]. *)
val replace_panel: Panel.t -> t -> t -> t option

(** Remove a panel.

    Return [None] if panel was not found in layout or if it is the only panel.
    Also return the panel which was next to the removed panel. *)
val remove_panel: Panel.t -> t -> (t * Panel.t) option
