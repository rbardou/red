let handlers = ref []

let add_handler (handler: string -> unit) =
  handlers := handler :: !handlers

let info m = List.iter (fun handler -> handler m) !handlers
let info x = Printf.ksprintf info x

let warn m = info "WARNING: %s" m
let warn x = Printf.ksprintf warn x

let error ?exn message =
  match exn with
    | None ->
        info "ERROR: %s" message
    | Some exn ->
        info "ERROR: %s: %s" message (Printexc.to_string exn)
let error ?exn x = Printf.ksprintf (error ?exn) x
