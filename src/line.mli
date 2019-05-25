(** Immutable text lines. *)

include module type of Sequence

(** Same as [count]. *)
val length: 'a t -> int

(** Make a line from a UTF8-encoded string. *)
val of_utf8_string: string -> Character.t t

(** Add a line to a buffer. *)
val to_buffer: Buffer.t -> Character.t t -> unit

(** Output a line to a channel, newline character not included. *)
val output_channel: out_channel -> Character.t t -> unit
