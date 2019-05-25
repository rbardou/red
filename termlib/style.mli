(** Rendering styles. *)

(** Style of a cell. *)
type t =
  {
    intensity: Term.intensity;
    underline: bool;
    fg: Term.color;
    bg: Term.color;
  }

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
