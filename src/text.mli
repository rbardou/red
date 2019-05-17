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
