(** Trees of characters to represent multisets of words. *)

(** Words. *)
type word = Character.t list

(** Tries. *)
type t

(** The empty trie. *)
val empty: t

(** Test whether a trie is empty, i.e. contains no word. *)
val is_empty: t -> bool

(** Add an occurrence of a word to a trie. *)
val add: word -> t -> t

(** Remove an occurrence of a word from a trie. *)
val remove: word -> t -> t

(** Get a subtrie.

    [get prefix trie] returns the trie of [suffix]es such that [prefix @ suffix] is in [trie]. *)
val get: word -> t -> t

(** Get the number of occurrences of a particular word. *)
val get_count: word -> t -> int

(** Iterate on all words which occur at least once.

    The second argument of the function is the number of occurrences. *)
val foreach: ?prefix: word -> t -> (word -> int -> unit) -> unit

(** Get the list word occurrences in a trie. *)
val to_list: t -> (word * int) list

(** Select the best word from a list of suffixes for autocompletion. *)
val best_for_autocompletion: t -> word
