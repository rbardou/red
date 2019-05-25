(** Output rendering for terminals. *)

(** Cell of a frame. *)
type cell =
  {
    character: string;
    style: Style.t;
  }

(** A cell with default style and character [" "]. *)
val empty_cell: cell

(** Make a cell.

    Default style is {!default}. *)
val cell: ?style: Style.t -> string -> cell

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

    Usage: [text frame x y width text]

    If text is longer than [width] printable characters, it is truncated.
    If it is smaller, it is extended with spaces. *)
val text: ?style: Style.t -> frame -> int -> int -> int -> string -> unit

(** Same as [text], but using a format string. *)
val textf: ?style: Style.t -> frame -> int -> int -> int ->
  ('a, unit, string, unit) format4 -> 'a
