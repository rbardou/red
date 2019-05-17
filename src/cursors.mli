(** Sets of cursors. *)

(** Sets of cursors. *)
type t

(** The empty set of cursors. *)
val empty: t

(** Add a new cursor to a set. *)
val add: int -> int -> t -> t

(** A set of cursors with exactly one cursor. *)
val one: int -> int -> t

type cursor =
  {
    selection_start_x: int;
    selection_start_y: int;
    x: int;
    y: int;
    clipboard: Text.t ref;
  }

(** Return whether a point is in the selection of a cursor. *)
val is_in_selection: int -> int -> cursor -> bool

(** Get the list of cursors which overlap a given interval,
    sorted by starting position. *)
val get: x1: int -> y1: int -> x2: int -> y2: int -> t -> cursor list

(** Move cursors after text has been inserted.

    Move cursors as if [lines] lines were inserted after [x, y]
    (their length does not matter, cursors move by this many lines)
    and [characters] characters were inserted after those lines.

    For instance, adding 2 lines and 10 characters means that
    line [y] becomes the beginning of [y] up to [x], plus the first new line;
    a new line is added after that; and 10 characters are added at the beginning
    of the line which was at [y + 1] and is now at [y + 2]. *)
val insert: x: int -> y: int -> characters: int -> lines: int -> t -> t

(** Move cursors after text has been deleted.

    Move cursors as if [lines] lines were deleted after [(x, y)],
    including the end of [y], and [characters] characters were deleted
    after those lines.

    For instance, deleting 2 lines and 10 characters means that
    the end of line [y] is deleted, that line [y + 1] is deleted,
    and that the first 10 characters of line [y + 2] are deleted. *)
val delete: x: int -> y: int -> characters: int -> lines: int -> t -> t
