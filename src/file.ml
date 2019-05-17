type t =
  {
    mutable views: view list;
    mutable text: Text.t;
    mutable filename: string option;

    (* If [loading] is [Some (loaded, size, sub_strings_rev)], only [loaded] bytes out of [size]
       have been loaded, and [text] is read-only. *)
    mutable loading: (int * int * (string * int) list) option;
  }

and view =
  {
    file: t;
    mutable scroll_x: int;
    mutable scroll_y: int;
    mutable cursors: Cursors.t;
  }

let create_view file =
  let view =
    {
      file;
      scroll_x = 0;
      scroll_y = 0;
      cursors = Cursors.one 0 0;
    }
  in
  file.views <- view :: file.views;
  view

let create text =
  {
    views = [];
    text;
    filename = None;
    loading = None;
  }

let create_loading filename =
  let file_descr =
    (* TODO: error handling (e.g. file does not exist) *)
    Unix.openfile filename [ O_RDONLY ] 0o640
  in
  let size =
    (* TODO: error handling *)
    (Unix.fstat file_descr).st_size
  in

  let file = create Text.empty in
  file.filename <- Some filename;
  file.loading <- Some (0, size, []);

  let rec load () =
    (* TODO: group, so that if we kill this file, we kill the reader *)
    Spawn.on_read file_descr @@ fun () ->
    match file.loading with
      | None ->
          assert false (* did someone else temper with this file? *)
      | Some (loaded, size, sub_strings_rev) ->
          let bytes = Bytes.create 8192 in
          let len = Unix.read file_descr bytes 0 8192 in
          if len = 0 then
            (* TODO: [Text.of_utf8_substrings_offset_0] can block for a while.
               Replace it by some kind of iterative parser. *)
            let text = Text.of_utf8_substrings_offset_0 (List.rev sub_strings_rev) in
            file.text <- text;
            file.loading <- None
          else
            let sub_strings_rev = (Bytes.unsafe_to_string bytes, len) :: sub_strings_rev in
            file.loading <- Some (loaded + len, size, sub_strings_rev);
            load ()
  in
  load ();

  file

let is_read_only file =
  match file.loading with
    | None ->
        false
    | Some _ ->
        true
