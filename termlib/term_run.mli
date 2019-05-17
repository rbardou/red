(** High-level main loop for terminal interaction. *)

(** Spawn a task which parses [stdin].

    Call [on_key_press] for each received key press.

    Call [on_size_changed] when terminal size changed.
    Note that you should not call [size_changed] yourself if you use [spawn_input_reader].

    Call [on_cursor_position] for each received cursor position, that
    you can query with [query_cursor_position].

    Call [on_sleep] before waiting for more input.

    You still need to run [Spawn.run] yourself. *)
val spawn_input_reader:
  on_key_press: (Key.t -> unit) ->
  ?on_size_changed: (unit -> unit) ->
  ?on_cursor_position: (int -> int -> unit) ->
  ?state: Term.input_state ->
  unit -> unit

(** High-level main loop.

    Call [on_key_press] for each received key press.

    Call [on_render] with the current terminal size
    before waiting for new key presses.

    You should not call [size_changed] yourself if you use [run_raw_mode].
    Note that when the terminal size changes, the cursor position is set to the
    bottom-right in order to query the new terminal size. So when [on_render] is
    called, you cannot assume the current cursor position is still where you left it. *)
val run_raw_mode:
  on_key_press: (Key.t -> unit) ->
  ?on_render: (Render.frame -> unit) ->
  unit -> unit
