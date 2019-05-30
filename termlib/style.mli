(** Rendering styles. *)

(** Style of a cell. *)
type t =
  {
    intensity: Term.intensity;
    underline: bool;
    fg: Term.color;
    bg: Term.color;
  }

(** Convert a style to a string, for debugging. *)
val show: t -> string

(** Default style with normal intensity, no underline, default colors. *)
val default: t

(** Revert foreground and background colors. *)
val revert: t -> t

(** Make a style.

    Default values are taken from {!default}. *)
val make:
  ?intensity: Term.intensity ->
  ?underline: bool ->
  ?fg: Term.color ->
  ?bg: Term.color ->
  unit -> t

(** Same as [make ~intensity: Normal]. *)
val normal:
  ?underline: bool ->
  ?fg: Term.color ->
  ?bg: Term.color ->
  unit -> t

(** Same as [make ~intensity: Bold]. *)
val bold:
  ?underline: bool ->
  ?fg: Term.color ->
  ?bg: Term.color ->
  unit -> t

(** Same as [make ~intensity: Faint]. *)
val faint:
  ?underline: bool ->
  ?fg: Term.color ->
  ?bg: Term.color ->
  unit -> t

(** Same as [make], but randomize unspecified fields. *)
val random:
  ?intensity: Term.intensity ->
  ?underline: bool ->
  ?fg: Term.color ->
  ?bg: Term.color ->
  unit -> t

(** Same as [make ~fg ()]. *)
val fg: Term.color -> t

(** Same as [make ~bg ()]. *)
val bg: Term.color -> t
