(** Immutable text lines. *)

(** Immutable texts, i.e. sequences of characters. *)
type t

(** An line with no characters. *)
val empty: t

(** Make a line from a list of characters. *)
val of_list: Character.t list -> t

(** Get a character from its index in a line. *)
val get: int -> t -> Character.t option

(** Get the length of a line. *)
val length: t -> int
