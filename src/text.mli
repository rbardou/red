(** Immutable texts. *)

(** Immutable texts, i.e. sequences of lines. *)
type t

(** A text with only one empty line. *)
val empty: t

(** A text with only one non-empty line. *)
val one_line: Line.t -> t

(** Output to file.

    May raise [System.Error]. *)
val output_file: ?perm: Unix.file_perm -> string -> t -> unit

(** Convert to a string. *)
val to_string: t -> string

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

(** Insert a new line at a given position.

    Usage: [insert (x, y) text] *)
val insert_new_line: int -> int -> t -> t

(** Delete a region.

    Start at [x, y].
    Remove until [lines] newline characters, then [characters] more characters. *)
val delete_region: x: int -> y: int -> characters: int -> lines: int -> t -> t

(** Get a subpart of a text.

    [(x2, y2)] should be after [(x1, y1)]. *)
val sub: x1: int -> y1: int -> x2: int -> y2: int -> t -> t

(** Insert a text inside a text. *)
val insert_text: x: int -> y: int -> sub: t -> t -> t

(** Append a character (which may in particular be [\n] to create a new line) to a text. *)
val append_character: Character.t -> t -> t
