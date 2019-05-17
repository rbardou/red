type cursor =
  {
    selection_start_x: int;
    selection_start_y: int;
    x: int;
    y: int;
    preferred_x: int;
    clipboard: Text.t ref;
  }

(* TODO: We could use interval trees to be more efficient, maybe.
   But if we assume that there are not that many cursors it does
   not matter much, probably. *)
type t = cursor list

let empty = []

let add x y cursors =
  let cursor =
    {
      selection_start_x = x;
      selection_start_y = y;
      x;
      y;
      preferred_x = x;
      clipboard = ref Text.empty;
    }
  in
  cursor :: cursors

let one x y =
  add x y empty

(* Test whether [(x1, y1)] is before [(x2, y2)].
   Basically defines a lexicographic ordering, so we could just use [compare]. *)
let (<%) (x1, y1) (x2, y2) = y1 < y2 || (y1 = y2 && x1 < x2)
let (<=%) (x1, y1) (x2, y2) = y1 < y2 || (y1 = y2 && x1 <= x2)

let selection_boundaries cursor =
  let selection_start = cursor.selection_start_x, cursor.selection_start_y in
  let cursor = cursor.x, cursor.y in
  if cursor <% selection_start then
    cursor, selection_start
  else
    selection_start, cursor

let is_in_selection x y cursor =
  let left, right = selection_boundaries cursor in
  let xy = x, y in
  left <=% xy && xy <% right

let get ~x1 ~y1 ~x2 ~y2 cursors =
  let xy1 = x1, y1 in
  let xy2 = x2, y2 in
  let overlaps cursor =
    let left, right = selection_boundaries cursor in
    (* Intervals do not intersect if: low2 > up1 || up2 < low1 *)
    left <=% xy2 && xy1 <=% right
  in
  List.filter overlaps cursors

let map_marks f cursors =
  let map_cursor cursor =
    let new_sx, new_sy = f cursor.selection_start_x cursor.selection_start_y in
    let new_x, new_y = f cursor.x cursor.y in
    {
      selection_start_x = new_sx;
      selection_start_y = new_sy;
      x = new_x;
      y = new_y;
      preferred_x = new_x;
      clipboard = cursor.clipboard;
    }
  in
  List.map map_cursor cursors

let insert ~x ~y ~characters ~lines cursors =
  let xy = x, y in
  let move_mark mx my =
    let mark = mx, my in
    (* TODO: if inserting at mark, does it depend on which mark we are moving? *)
    if mark < xy then
      (* Inserting after mark: do not move mark. *)
      mark
    else (
      (* Inserting before or at mark: move mark. *)
      if my = y then
        (* Inserting on the same line as the mark; x coordinate changes.

           Example 1: inserting XXX at | (i.e. [lines] is 0 and [characters] is 3):

               -------|-------M--------

           Becomes:

               -------|XXX-------M--------

           Example 2: inserting XXX\nYYYY\nZ at | (i.e. [lines] is 2 and [characters] is 1):

               -------|-------M--------

           Becomes:

               -------|XXX\n
               YYYY\n
               Z-------M--------

           Only the length of Z (i.e. [characters]) line matters for the x coordinate,
           not the length of XXX and YYYY. *)
        mx - x + characters, my + lines
      else
        (* Inserting on a previous line; x coordinate does not change. *)
        mx, my + lines
    )
  in
  map_marks move_mark cursors

let delete ~x ~y ~characters ~lines cursors =
  (* Beginning of the deleted region. *)
  let xy = x, y in

  (* End of the deleted region. *)
  let (_, y2) as xy2 =
    if lines = 0 then
      x + characters, y
    else
      characters, y + lines
  in

  let move_mark mx my =
    (* For a given mark, either the mark is:
       - before the removed region, in which case the mark does not move;
       - inside the removed region, in which case the mark moves to the beginning of this removed region;
       - after the removed region, in which case the mark moves like when inserting but in reverse. *)
    let mark = mx, my in
    if mark < xy then
      (* Deleting after mark: do not move mark. *)
      mark
    else if xy <= mark && mark <= xy2 then
      (* Mark is inside deleted region: move mark to the beginning of the deleted region. *)
      xy
    else (
      (* Mark is after deleted region: move it. *)
      if my = y2 then
        (* Deleting on the same line as the mark; x coordinate changes.

           Exemple 1: deleting XXX from a single line (i.e. [lines] is 0 and [characters] is 3):

               -------XXX-----M------

           Becomes:

               ------------M------

           Example 2: deleting XXX\nYYYY\nZ (i.e. [lines] is 2 and [characters] is 1):

               -------XXX\n
               YYYY\n
               Z-----M------

           Becomes:

               ------------M------

           One again, the length of XXX and YYYY does not matter. *)
        mx - characters + x, my - lines
      else
        (* Deleting lines which are strictly before the mark; x coordinate does not change. *)
        mx, my - lines
    )
  in
  map_marks move_mark cursors

let map f cursors =
  List.map f cursors
