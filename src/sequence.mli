(** Balanced binary trees to represent sequences of items.

    Items can be inserted or deleted efficiently given their index. *)

(** Sequences. *)
type +'a t

(** Show a sequence, for debugging. *)
val show: ('a -> string) -> 'a t -> string

(** Pretty-print a sequence, for debugging. *)
val pretty: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** The empty sequence. *)
val empty: 'a t

(** Return whether a sequence is empty. *)
val is_empty: 'a t -> bool

(** Negation of [is_empty]. *)
val is_not_empty: 'a t -> bool

(** Return the size of a sequence. *)
val count: 'a t -> int

(** Get a value given its index in a sequence. *)
val get: int -> 'a t -> 'a option

(** Set the value at a given index in a sequence. *)
val set: int -> 'a -> 'a t -> 'a t

(** Insert a value before a given index in a sequence.

    If index is less than [0], insert at [0] instead.

    If index is greater than the size of the sequence, insert at the
    right-most position instead. *)
val insert: int -> 'a -> 'a t -> 'a t

(** Add a value at the beginning of a sequence. *)
val prepend: 'a -> 'a t -> 'a t

(** Add a value at the end of a sequence. *)
val append: 'a -> 'a t -> 'a t

(** Remove a value given its index.

    If index is out of bounds, do nothing. *)
val remove: int -> 'a t -> 'a t

(** Make a sequence of size one. *)
val one: 'a -> 'a t

(** Make a sequence from an array.

    More efficient than using [append] repeatedly: O(n) instead of O(n * log(n)).

    Parameter [ofs] (offset) defines the starting index in the array. Default is 0.

    Parameter [len] defines the number of items to add starting from [ofs].
    Default is the length of the array minus [ofs]. *)
val of_array: ?ofs: int -> ?len: int -> 'a array -> 'a t

(** Same as [of_array] but for lists. *)
val of_list: ?ofs: int -> ?len: int -> 'a list -> 'a t

(** Convert a sequence into a list. *)
val to_list: 'a t -> 'a list

(** Iterate over values in index order. *)
val iter: ('a -> unit) -> 'a t -> unit

(** Apply a function to all values. *)
val map: ('a -> 'b) -> 'a t -> 'b t

(** Apply a function to a range of values.

    Usage: [map_sub first last]. *)
val map_sub: int -> int -> ('a -> 'a) -> 'a t -> 'a t

(** Apply a function until a given character.

    Usage: [map_left last]. *)
val map_until: int -> ('a -> 'a) -> 'a t -> 'a t

(** Apply a function from a given character.

    Usage: [map_left first]. *)
val map_from: int -> ('a -> 'a) -> 'a t -> 'a t

(** Fold over values in index order. *)
val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Split a sequence at a given index.

    Given index is put in the right-hand side.
    Index can be from zero to the sequence length. *)
val split: int -> 'a t -> 'a t * 'a t

(** Split a sequence at a given index and return the left-hand side. *)
val split_left: int -> 'a t -> 'a t

(** Split a sequence at a given index and return the right-hand side. *)
val split_right: int -> 'a t -> 'a t

(** Concatenate two sequences. *)
val concat: 'a t -> 'a t -> 'a t

(** Get a subpart of a sequence.

    Usage: [sub first last sequence] *)
val sub: int -> int -> 'a t -> 'a t

(** Insert a sequence into another.

    Usage: [insert_sub x sequence_to_insert sequence]

    Similar to {!split}ting [sequence] into [left, right] at [x],
    then concatenating [left], [sequence_to_insert] and [right],
    but more efficient if [sequence_to_insert] is small. *)
val insert_sub: int -> 'a t -> 'a t -> 'a t

(** Test whether two sequences are equal, given a way to compare characters. *)
val equals: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Removing items at the end if a sequence contains too many.

    Usage: [truncate max_count] *)
val truncate: int -> 'a t -> 'a t
