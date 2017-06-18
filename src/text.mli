(** Text buffers. *)

(** One-width UTF-8 characters. *)
type character = string

(** Text buffers. *)
type t

(** The empty text buffer. *)
val empty: t

(** Load text file. *)
val load_file: string -> t

(** Save text file. *)
val save_file: string -> t -> unit

(** Get a character at a given position.

    Usage: [get (x, y) text]

    Return character at line [y], column [x].
    Both [x] and [y] start at [0]. *)
val get: int * int -> t -> character

(** Get number of lines. *)
val line_count: t -> int

(** Get length of a given line.

    Usage: [line_length y text] *)
val line_length: int -> t -> int

(** Insert a character at a given position.

    Usage: [insert (x, y) character text] *)
val insert: int * int -> character -> t -> t

(** Split a line at a given position by inserting a new line after it.

    Usage: [split_line (x, y) text] *)
val split_line: int * int -> t -> t

(** Remove a character at a given position.

    Usage: [insert (x, y) text] *)
val remove: int * int -> t -> t

(** Remove a line. *)
val remove_line: int -> t -> t

(** Concatenate two consecutive lines into one.

    Usage: [merge_lines y text]

    Merge lines [y] and [y + 1]. *)
val merge_lines: int -> t -> t

(** Get a subpart of a text. *)
val sub: int * int -> int * int -> t -> t

(** Insert a text inside a text.

    Usage: [insert_text (x, y) text_to_insert text] *)
val insert_sub: int * int -> t -> t -> t
