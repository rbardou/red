(** Immutable text lines. *)

(** Immutable texts, i.e. sequences of characters. *)
type t

(** A line with no characters. *)
val empty: t

(** Make a line with one character. *)
val one: Character.t -> t

(** Make a line from a list of characters. *)
val of_list: Character.t list -> t

(** Make a line from a UTF8-encoded string. *)
val of_utf8_string: string -> t

(** Add a line to a buffer. *)
val to_buffer: Buffer.t -> t -> unit

(** Get a character from its index in a line. *)
val get: int -> t -> Character.t option

(** Get the length of a line. *)
val length: t -> int

(** Insert a character into a line. *)
val insert: int -> Character.t -> t -> t

(** Split a line in two. *)
val split: int -> t -> t * t

(** Split a line and return the left-hand side. *)
val split_left: int -> t -> t

(** Split a line and return the right-hand side. *)
val split_right: int -> t -> t

(** Concatenate two lines. *)
val concat: t -> t -> t

(** Output a line to a channel, newline character not included. *)
val output_channel: out_channel -> t -> unit

(** Get part of a line.

    Usage: [sub first last line] *)
val sub: int -> int -> t -> t
