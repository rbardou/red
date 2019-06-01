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

(** Set the current viewed file of a panel.

    If there is already a view for this file in this panel, use it.
    Else, copy a view from the file if it has one.
    Else, create a new view.

    New view has kind [File]. *)
val set_current_file: t -> File.t -> unit

(** Switch to previous view and forget about the current one.

    Cannot do that with the last view.
    Return [false] if this was the last view. *)
val kill_current_view: t -> bool

(** Remove a file from a panel.

    If panel is viewing this file, switch to another view.
    If there is no other view, use the one given by the function given as second argument. *)
val remove_file: File.t -> (unit -> File.view) -> t -> unit

(** Create a panel with an initial view. *)
val create: File.view -> t

(** Create a panel with an initial view created from a file. *)
val create_file: File.t -> t

(** Render a panel.

    First argument is the panel which currently has the focus. *)
val render: t -> Render.frame -> t -> x: int -> y: int -> w: int -> h: int -> unit
