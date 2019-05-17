(** Immutable texts. *)

(** Immutable texts, i.e. sequences of lines. *)
type t

(** A text with only one empty line. *)
val empty: t

(** Make a text from a list of substrings.

    Each substring is of the form: [(string, len)]
    (the offset is implicitely [0]). *)
val of_utf8_substrings_offset_0: (string * int) list -> t

(** Get a character at a given position.

    Usage: [get x y text]

    Return character at line [y], column [x].
    Both [x] and [y] start at [0]. *)
val get: int -> int -> t -> Character.t option

(** Get the number of lines in a text.

    Usage: [get_line_count y text] *)
val get_line_count: t -> int

(** Get length of a given line.

    Usage: [get_line_length y text] *)
val get_line_length: int -> t -> int

(** Insert a character at a given position.

    Usage: [insert (x, y) character text] *)
val insert_character: int -> int -> Character.t -> t -> t
