(** Spawn cooperative threads and run them. *)

(** {2 Groups} *)

(** Collections of tasks. *)
type group

(** The default group for new tasks. *)
val default_group: group

(** Create a new empty collection of tasks.

    Subtasks (tasks spawned by other tasks) belong to the group of their parent by default.
    Root tasks (tasks which are spawned outside of any task) belong to [default]. *)
val group: unit -> group

(** Cancel all tasks of a given collection.

    If you try to add a task to a dead group, the task will just be ignored.

    Warning: if a task is responsible of cleaning up something
    (like closing open file descriptors or terminating open processes),
    killing it will prevent the cleanup. So don't have tasks be responsible of
    cleaning up if you intend to potentially kill them.

    Note that this does not actually remove the tasks immediately from the list
    of pending task. They are removed when the scheduler runs. *)
val kill: group -> unit

(** Return whether a group is still active, or if it has been killed. *)
val is_active: group -> bool

(** {2 Low-Level Tasks} *)

(** Spawn a task to be executed as soon as possible but not immediately. *)
val task: ?group: group -> (unit -> unit) -> unit

(** Spawn a task to be executed at a given time (as returned by [Unix.gettimeofday]). *)
val alarm: ?group: group -> float -> (unit -> unit) -> unit

(** Spawn a task to be executed after a given time interval (in seconds). *)
val sleep: ?group: group -> float -> (unit -> unit) -> unit

(** Spawn a task to be executed as soon as a file descriptor can be read without blocking.

    This includes reading "end of file". *)
val on_read: ?group: group -> Unix.file_descr -> (unit -> unit) -> unit

(** Spawn a task to be executed as soon as a file descriptor can be written without blocking.

    Writing too much data may still block. *)
val on_write: ?group: group -> Unix.file_descr -> (unit -> unit) -> unit

(** {2 Execution} *)

(** Run one iteration of the scheduler.

    The scheduler does not catch exceptions. If an exception is raised by a task, this may cause
    other tasks to be arbitrarily killed, so you should stop the program immediately. *)
val run_once: unit -> unit

(** Run the scheduler until there is no pending task. *)
val run: ?on_iterate: (unit -> unit) -> unit -> unit

(* (\** {2 File I/O} *\) *)

(* (\** File descriptors, open for reading. *\) *)
(* type file_in *)

(* (\** File descriptors, open for writing. *\) *)
(* type file_out *)

(* (\** Standard input. *\) *)
(* val stdin: file_in *)

(* (\** Standard output. *\) *)
(* val stderr: file_out *)

(* (\** Standard output. *\) *)
(* val stderr: file_out *)

(* (\** Open a file for reading. *\) *)
(* val open_in: string -> file_in *)

(* (\** Open a file for writing. *)

(*     Default value for [if_exists] is [`fail]. *\) *)
(* val open_out: ?if_exists: [ `fail | `truncate | `append ] -> string -> file_out *)

(* (\** Read some bytes from a file. *)

(*     [min] is the minimum number of bytes to read before calling the continuation. *)
(*     Bytes are buffered until that many bytes have been read. *)
(*     Note that in case of end of file, the number of bytes can still be less than [min_length]. *)
(*     Default is [1]. *)

(*     [max] is the maximum number of bytes to read. *)
(*     Must be at least equal to [min_length]. *)
(*     Default is [1024]. *\) *)
(* val read: ?min: int -> ?max: int -> file_in -> ((string, Unix.error) result -> unit) -> unit *)

(* (\** Write some bytes to a file. *)

(*     If an error occurs, it is guaranteed that no bytes have been written. *\) *)
(* val write: file_out -> string -> ((unit, Unix.error) result -> unit) -> unit *)

(* (\** {2 High-Level} *\) *)

(* (\** Read some number of bytes. *)

(*     Bytes will be buffered until there is the requested amount, *)
(*     at which point the task will be triggered. *)

(*     If end of file is encountered before enough bytes were read, *)
(*     the task is executed with the bytes that could be read. *\) *)
(* val read_count *)
