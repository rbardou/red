module File_descr =
struct
  type t = Unix.file_descr
  let compare = (Pervasives.compare: Unix.file_descr -> Unix.file_descr -> int)
end

module File_descr_map =
struct
  include Map.Make (File_descr)
  let keys map = fold (fun key _ acc -> key :: acc) map []
  let values map = fold (fun _ value acc -> value :: acc) map []
end

(* Boolean says whether the group is alive. *)
type group = bool ref

let group () = ref true

let default_group = group ()
let current_group = ref default_group

let kill group = group := false

let is_active group = !group

type requirement =
  | Immediate
  | Time of float
  | Readable of Unix.file_descr
  | Writable of Unix.file_descr

type task =
  {
    group: group;
    requires: requirement;
    run: unit -> unit;
  }

let tasks = ref []

let add_task ?group requires run =
  let task =
    {
      group = (match group with None -> !current_group | Some g -> g);
      requires;
      run;
    }
  in
  tasks := task :: !tasks

let task ?group run =
  add_task ?group Immediate run

let alarm ?group time run =
  add_task ?group (Time time) run

let sleep ?group delay run =
  alarm ?group (Unix.gettimeofday () +. delay) run

let on_read ?group file run =
  add_task ?group (Readable file) run

let on_write ?group file run =
  add_task ?group (Writable file) run

let run_task task =
  let old_group = !current_group in
  current_group := task.group;
  task.run ();
  current_group := old_group

let run_once ?(on_wait = fun () -> ()) () =
  let now = Unix.gettimeofday () in

  (* Clear the list of tasks.
     New subtasks will be added to it, but won't be run until the next iteration.
     We will add back the tasks we could not run during this iteration. *)
  let tasks_to_run = !tasks in
  tasks := [];

  (* We will accumulate tasks that cannot be run immediately in these references. *)
  let timeout: float option ref = ref None in (* Time until the next alarm, if any. *)
  let pending_reads: task list File_descr_map.t ref = ref File_descr_map.empty in
  let pending_writes: task list File_descr_map.t ref = ref File_descr_map.empty in

  (* Run a task immediately or add it to the lists above (unless it has been killed). *)
  let try_to_run task =
    if is_active task.group then
      match task.requires with
        | Immediate ->
            run_task task
        | Time time ->
            if time <= now then
              run_task task
            else (
              timeout := (
                match !timeout with
                  | None ->
                      Some (time -. now)
                  | Some timeout ->
                      Some (min timeout (time -. now))
              );
              tasks := task :: !tasks
            )
        | Readable file ->
            let existing =
              match File_descr_map.find file !pending_reads with
                | exception Not_found -> []
                | l -> l
            in
            pending_reads := File_descr_map.add file (task :: existing) !pending_reads
        | Writable file ->
            let existing =
              match File_descr_map.find file !pending_writes with
                | exception Not_found -> []
                | l -> l
            in
            pending_writes := File_descr_map.add file (task :: existing) !pending_writes
  in
  List.iter try_to_run tasks_to_run;

  (* Finalize pending values. *)
  let timeout =
    match !tasks with
      | [] ->
          (
            match !timeout with
              | None ->
                  -1.
              | Some timeout ->
                  timeout
          )
      | _ :: _ ->
          (* If there are new tasks, don't wait before the next iteration. *)
          0.
  in
  let pending_reads = !pending_reads in
  let pending_writes = !pending_writes in

  (* Wait as needed until some file descriptors are ready. *)
  on_wait ();
  let ready_to_read, ready_to_write, _ =
    if
      not (File_descr_map.is_empty pending_reads) ||
      not (File_descr_map.is_empty pending_writes) ||
      timeout > 0.
    then
      try
        Unix.select
          (File_descr_map.keys pending_reads)
          (File_descr_map.keys pending_writes)
          []
          timeout
      with Unix.Unix_error (Unix.EINTR, _, _) ->
        (* Maybe interrupted because of a signal or something. *)
        [], [], []
    else
      [], [], []
  in

  (* Execute tasks with a file descriptor which is ready. *)
  let run_ready pending ready =
    match File_descr_map.find ready pending with
      | exception Not_found ->
          (* Unix.select returned a file descriptor that we did not ask??
             This is more likely a programming error on our side. *)
          assert false
      | l ->
          List.iter (fun task -> run_task task) l;
          File_descr_map.remove ready pending
  in
  let remaining_reads = List.fold_left run_ready pending_reads ready_to_read in
  let remaining_writes = List.fold_left run_ready pending_writes ready_to_write in

  (* Add remaining tasks back. *)
  tasks :=
    List.flatten [
      List.flatten (File_descr_map.values remaining_reads);
      List.flatten (File_descr_map.values remaining_writes);
      !tasks;
    ]

let rec run ?on_wait () =
  match !tasks with
    | [] ->
        ()
    | _ :: _ ->
        run_once ?on_wait ();
        run ?on_wait ()
