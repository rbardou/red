(** Panel layouts. *)

(** How to split a layout. *)
type split_direction =
  | Vertical (** top and bottom *)
  | Horizontal (** left and right *)

(** Where to split a layout. *)
type split_position =
  | Absolute_first of int (** set the size of the top or left sublayout, and the other one takes the rest *)
  | Absolute_second of int (** set the size of the bottom or right sublayout, and the other one takes the rest *)
  | Ratio of int * int (** numerator, denominator: denotes a fraction of the parent's size *)

(** The main sublayout to pick with [get_main_panel]. *)
type split_main =
  | First
  | Second

(** Panel layouts. *)
type t

(** A layout with a single panel. *)
val single: Panel.t -> t

(** A layout split in two. *)
val split: split_direction -> ?pos: split_position -> ?sep: bool -> ?main: split_main -> t -> t -> t

(** Get the main panel of a layout. *)
val get_main_panel: t -> Panel.t

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

(** Return whether a panel is visible in a layout. *)
val panel_is_visible: Panel.t -> t -> bool
