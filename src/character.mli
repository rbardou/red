(** UTF8-encoded characters. *)

(** Characters which should be displayed in exactly one cell. *)
type t = string

(** Make a character from an ASCII [char]. *)
val of_ascii: char -> t

(** Test whether a character is a character that can be part of a word.

    Letters or digits are word characters.
    Symbols, such as [+], [%], [-] or [_], are not. *)
val is_word_character: t -> bool

(** Negation of [is_word_character]. *)
val is_not_word_character: t -> bool
