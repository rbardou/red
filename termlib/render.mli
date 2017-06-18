(** Output rendering for terminals. *)

(** Style of a cell. *)
type style =
  {
    intensity: Term.intensity;
    underline: bool;
    fg_color: Term.color;
    bg_color: Term.color;
  }

(** Default style with normal intensity, no underline, default colors. *)
val default: style

(** Revert foreground and background colors. *)
val revert: style -> style

(** Make a [style].

    Default values are taken from {!default}. *)
val style:
  ?intensity: Term.intensity ->
  ?underline: bool ->
  ?fg_color: Term.color ->
  ?bg_color: Term.color ->
  unit -> style

(** Cell of a frame. *)
type cell =
  {
    character: string;
    style: style;
  }

(** A cell with default style and character [" "]. *)
val empty_cell: cell

(** Make a cell.

    Default style is {!default}. *)
val cell: ?style: style -> string -> cell

(** A frame to render on terminal.

    [grid] is an array of [height] lines, where each line is an array
    of [width] cells. *)
type frame

(** A frame with [0] width and [0] height. *)
val empty: frame

(** Get the width of a frame. *)
val width: frame -> int

(** Get the height of a frame. *)
val height: frame -> int

(** Create a frame with empty cells.

    Usage: [create_frame width height] *)
val create_frame: int -> int -> frame

(** Set a cell of a frame.

    Usage: [set frame x y cell] *)
val set: frame -> int -> int -> cell -> unit

(** Get a cell of a frame.

    Usage: [get frame x y] *)
val get: frame -> int -> int -> cell

(** Output a frame to [stdout] and flush [stdout].

    If [previous_frame] is specified, assume terminal still contains
    [previous_frame] and only output the difference. Else, clear terminal. *)
val output: ?previous_frame: frame -> frame -> unit

(** Render text to a frame.

    Usage: [text_to_frame frame x y width text]

    If text is longer than [width] printable characters, it is truncated.
    If it is smaller, it is extended with spaces. *)
val text_to_frame: ?style: style -> frame -> int -> int -> int -> string -> unit
