(** Immutable text lines. *)

(** Immutable texts, i.e. sequences of characters. *)
type t

(** A line with no characters. *)
val empty: t

(** Make a line with one character. *)
val one: Character.t -> t

(** Make a line from a list of characters. *)
val of_list: Character.t list -> t

(** Get a character from its index in a line. *)
val get: int -> t -> Character.t option

(** Get the length of a line. *)
val length: t -> int

(** Insert a character into a line. *)
val insert: int -> Character.t -> t -> t
