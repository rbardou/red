(** Terminal interaction. *)

(** {2 Raw Mode} *)

(** Set terminal settings to enter "raw mode".

    Don't forget to call [restore], or use [with_raw_mode] instead. *)
val raw_mode: unit -> unit

(** Restore terminal settings to what they were before {!raw_mode}.

    Also calls {!show_cursor}, {!reset_style}, {!goto_xy} to [1, 1],
    and {!clear}. *)
val restore: unit -> unit

(** Enter raw mode, run a function, and restore raw mode even if said
    function raises an exception. *)
val with_raw_mode: (unit -> 'a) -> 'a

(** Return [true] if terminal size changed since last call to {!size_changed}.

    Always return [true] at first call.
    Only works if [raw_mode] has been called. *)
val size_changed: unit -> bool

(** {2 Input Parsing} *)

(** State of an input parser automata. *)
type input_state =
  | Empty
  | Unicode of char list * int
  | Escape
  | Escape_unicode of char list * int
  | Escape_bracket of int
  | Escape_bracket_semi of int * int
  | Escape_O of int
  | Escape_O_semi of int * int
  | Done of Key.t
  | Cursor_position of int * int (** column, row *)
  | Invalid of input_state * char

(** Convert an input state to a string. *)
val show_input_state: input_state -> string

(** Parse one input char to update an input parser automata state. *)
val input_char: input_state -> char -> input_state

(** {2 Control Sequences} *)

(** {3 Cursor} *)

(** Remember to [flush stdout] so that the following sequences can take
    effect. *)

(** Set cursor position.

    Usage: [goto_xy column row]

    Column and row numbers start at [1], not [0]. *)
val goto_xy: int -> int -> unit

(** Set cursor position to bottom-right.

    Useful combined with {!query_cursor_position} to get terminal size. *)
val goto_bottom_right: unit -> unit

(** Query cursor position.

    The response will be given by a [Cursor_position] input that
    {!input_char} can parse. *)
val query_cursor_position: unit -> unit

(** Hide cursor. *)
val hide_cursor: unit -> unit

(** Show cursor. *)
val show_cursor: unit -> unit

(** {3 Clear} *)

(** Clear terminal from cursor to end. *)
val clear_from_cursor: unit -> unit

(** Clear terminal from beginning to cursor. *)
val clear_to_cursor: unit -> unit

(** Clear whole terminal. *)
val clear: unit -> unit

(** Clear whole terminal and scrollback. *)
val clear_scrollback: unit -> unit

(** {3 Style} *)

(** Reset style to default.

    This includes intensity (bold or faint), underline, and colors. *)
val reset_style: unit -> unit

(** Available intensities for {!intensity}. *)
type intensity =
  | Normal
  | Bold
  | Faint

(** Set text intensity. *)
val intensity: intensity -> unit

(** Set whether text is underlined. *)
val underline: bool -> unit

(** Available colors for {!fg_color} and {!bg_color}. *)
type color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

(** Set foreground (text) color. *)
val fg_color: color -> unit

(** Set background color. *)
val bg_color: color -> unit
