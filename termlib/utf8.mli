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
