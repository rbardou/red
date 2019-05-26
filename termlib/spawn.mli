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
    other tasks to be arbitrarily killed, so you should stop the program immediately.

    [on_wait] is called after tasks which could be run immediately have been run,
    i.e. just before waiting on file descriptors. It is called even if there is nothing to wait for. *)
val run_once: ?on_wait: (unit -> unit) -> unit -> unit

(** Run the scheduler until there is no pending task. *)
val run: ?on_wait: (unit -> unit) -> unit -> unit
