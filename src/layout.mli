type split_position
type t

val single: Panel.t -> t
val vertical_split: ?pos: split_position -> ?line: bool -> t -> t -> t
val horizontal_split: ?pos: split_position -> ?line: bool -> t -> t -> t

val get_first_panel: t -> Panel.t

val render:
  (Render.frame -> Panel.t -> x: int -> y: int -> w: int -> h: int -> unit) ->
  Render.frame -> ?x: int -> ?y: int -> w: int -> h: int -> t -> unit
