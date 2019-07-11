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

(** Test whether a character is a character that can be part of a big word.

    Big words are like words, except that [_] is also a big word character. *)
val is_big_word_character: t -> bool

(** Negation of [is_big_word_character]. *)
val is_not_big_word_character: t -> bool

(** Test whether two characters are equal. *)
val equals: t -> t -> bool

(** Test whether two characters are equal (case-insensitive). *)
val case_insensitive_equals: t -> t -> bool

(** Compare two characters. *)
val compare: t -> t -> int

module Set:
sig
  include Set.S with type elt = t
  val ascii_range: char -> char -> t
end

module Map: Map.S with type key = t
