(** Simple UTF8 decoding. *)

(** Split into a list of one-width characters.

    [invalid_rune] is used to replace invalid runes.
    If [invalid_rune] is [None], invalid runes are discarded.
    Default is [Some "�"].

    TODO: there may be characters which are displayed using a width
    of more than one.

    TODO: there may be characters which will be combined to become one
    characters, because of modifiers. *)
val split_runes: ?invalid_rune: string option -> string -> string list

(** Incremental parser. *)
type parser_state =
  | Started
  | Missing_1_of_2 of char
  | Missing_2_of_3 of char
  | Missing_1_of_3 of char * char
  | Missing_3_of_4 of char
  | Missing_2_of_4 of char * char
  | Missing_1_of_4 of char * char * char
  | Completed_ASCII of char
  | Completed_Unicode of string
  | Invalid

(** Continue parsing. *)
val add_char: char -> parser_state -> parser_state
