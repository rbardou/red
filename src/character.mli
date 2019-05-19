(** UTF8-encoded characters. *)

(** Characters which should be displayed in exactly one cell. *)
type t = string

(** Make a character from an ASCII [char]. *)
val of_ascii: char -> t
