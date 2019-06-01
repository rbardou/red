(** Parts of the screen which show file views. *)

(** Panels. *)
type t

(** Get the current view of a panel.

    If current view is a prompt, return the parent view. *)
val get_current_main_view: t -> File.view

(** Get the current view of a panel. *)
val get_current_view: t -> File.view

(** Set the current view of a panel. *)
val set_current_view: t -> File.view -> unit

(** Switch to previous view and forget about the current one.

    Cannot do that with the last view.
    Return [false] if this was the last view. *)
val kill_current_view: t -> bool

(** Create a panel with an initial view. *)
val create: File.view -> t

(** Create a panel with an initial view created from a file. *)
val create_file: File.t -> t

(** Render a panel.

    First argument is the panel which currently has the focus. *)
val render: t -> Render.frame -> t -> x: int -> y: int -> w: int -> h: int -> unit
