exception Error of string

let error ?exn reason =
  let reason =
    match exn with
      | None ->
          reason
      | Some (Sys_error sys_reason) ->
          reason ^ ": " ^ sys_reason
      | Some exn ->
          reason ^ ": " ^ Printexc.to_string exn
  in
  raise (Error reason)

let error ?exn x =
  Printf.ksprintf (error ?exn) x

let file_exists filename =
  try
    Sys.file_exists filename
  with exn ->
    error ~exn "failed to test existence of %S" filename

let find_temporary_filename base =
  let rec find index =
    if index >= 1000 then error "failed to find an unused temporary filename for %S" base;
    let candidate = base ^ ".tmp." ^ string_of_int index in
    if file_exists candidate then
      find (index + 1)
    else
      candidate
  in
  find 1

let remove filename =
  try
    Sys.remove filename
  with exn ->
    error ~exn "failed to remove %S" filename

let move_file source destination =
  if file_exists destination then (
    try
      remove destination
    with Error reason ->
      error "failed to remove %S to rename %S: %s" destination source reason
  );
  try
    Sys.rename source destination
  with exn ->
    error ~exn "failed to rename %S into %S" source destination

let with_open_out ?perm filename f =
  match
    match perm with
      | None ->
          open_out_bin filename
      | Some perm ->
          open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] perm filename
  with
    | exception exn ->
        error ~exn "failed to open %S for writing" filename
    | ch ->
        match f ch with
          | exception exn ->
              (
                match close_out ch with
                  | exception exn ->
                      Log.error ~exn "failed to close %S after error" filename
                  | () ->
                      ()
              );
              raise exn
          | result ->
              match close_out ch with
                | exception exn ->
                    error ~exn "failed to close %S after writing" filename
                | () ->
                    result

let ls directory =
  match Sys.readdir directory with
    | exception exn ->
        error ~exn "failed to read directory: %S" directory
    | filenames ->
        Array.to_list filenames |> List.filter @@ fun filename ->
        filename <> Filename.current_dir_name &&
        filename <> Filename.parent_dir_name

let is_directory filename =
  match Sys.is_directory filename with
    | exception Sys_error _ ->
        false
    | b ->
        b

let get_cwd () =
  Sys.getcwd ()
