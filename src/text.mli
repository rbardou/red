(** Immutable texts. *)

(** Immutable texts, i.e. sequences of lines. *)
type 'a t

(** A text with only one empty line. *)
val empty: 'a t

(** A text with only one non-empty line. *)
val one_line: 'a Line.t -> 'a t

(** Output to file.

    May raise [System.Error]. *)
val output_file: ?perm: Unix.file_perm -> string -> Character.t t -> unit

(** Convert to a string. *)
(* TODO: rename into to_utf8_string? or to_utf8?
   or assume UTF8 everywhere and rename of_utf8_string into of_string instead? *)
val to_string: Character.t t -> string

(** Convert from a string. *)
val of_utf8_string: string -> Character.t t

(** Make a text from a list of substrings.

    Each substring is of the form: [(string, len)]
    (the offset is implicitely [0]). *)
val of_utf8_substrings_offset_0: (string * int) list -> Character.t t

(** Get a character at a given position.

    Usage: [get x y text]

    Return character at line [y], column [x].
    Both [x] and [y] start at [0]. *)
val get: int -> int -> 'a t -> 'a option

(** Set a character at a given position.

    Usage: [set x y character text] *)
val set: int -> int -> 'a -> 'a t -> 'a t

(** Get a line given its index.

    Return the empty line if line does not exist. *)
val get_line: int -> 'a t -> 'a Line.t

(** Get the number of lines in a text.

    Usage: [get_line_count y text] *)
val get_line_count: 'a t -> int

(** Get length of a given line.

    Usage: [get_line_length y text] *)
val get_line_length: int -> 'a t -> int

(** Insert a character at a given position.

    Usage: [insert (x, y) character text] *)
val insert: int -> int -> 'a -> 'a t -> 'a t

(** Insert a new line at a given position.

    Usage: [insert (x, y) text] *)
val insert_new_line: int -> int -> 'a t -> 'a t

(** Delete a region.

    Start at [x, y].
    Remove until [lines] newline characters, then [characters] more characters. *)
val delete_region: x: int -> y: int -> characters: int -> lines: int -> 'a t -> 'a t

(** Get a subpart of a text.

    [(x2, y2)] should be after [(x1, y1)].
    Both [(x1, y1)] and [(x2, y2)] are included. *)
val sub: x1: int -> y1: int -> x2: int -> y2: int -> 'a t -> 'a t

(** Same as [sub], but expects [lines] and [characters]. *)
val sub_region: x: int -> y: int -> characters: int -> lines: int -> 'a t -> 'a t

(** Insert a text inside a text. *)
val insert_text: x: int -> y: int -> sub: 'a t -> 'a t -> 'a t

(** Append a character (which may in particular be [\n] to create a new line) to a text. *)
val append_character: Character.t -> Character.t t -> Character.t t

(** Insert an empty line at the end. *)
val append_new_line: 'a t -> 'a t

(** Apply a function to all characters. *)
val map: ('a -> 'b) -> 'a t -> 'b t

(** Apply a function to all values from [(x1, y1)] to [(x2, y2)]. *)
val map_sub: x1: int -> y1: int -> x2: int -> y2: int -> ('a -> 'a) -> 'a t -> 'a t

(** Concatenate two texts.

    The first line of the second text is appended to the last line of the first one. *)
val concat: 'a t -> 'a t -> 'a t

(** Test whether two texts are equal. *)
val equals: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Search for a subtext inside a text.

    Return [Some (start_x, start_y, end_x, end_y)] if found. *)
val search_forwards:
  ('a -> 'a -> bool) ->
  ?x1: int -> ?y1: int -> ?x2: int -> ?y2: int ->
  subtext: 'a t -> 'a t -> (int * int * int * int) option
