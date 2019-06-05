module String_map =
struct
  include Map.Make (String)

  let keys map =
    fold (fun key _ acc -> key :: acc) map [] |> List.rev
end

module String_set = Set.Make (String)

let filter_choices filter choices =
  let filters = Filter_lexer.items [] (Lexing.from_string filter) in
  let matches choice filter =
    (* TODO: case-sensitive match if filter has capital letters *)
    let choice = String.lowercase_ascii choice in
    let filter = String.lowercase_ascii filter in
    let choice_length = String.length choice in
    let filter_length = String.length filter in
    let rec loop start =
      if start + filter_length > choice_length then
        false
      else if String.sub choice start filter_length = filter then
        true
      else
        loop (start + 1)
    in
    loop 0
  in
  List.filter (fun (_, choice) -> List.for_all (matches choice) filters) choices

let rec truncate_list ?(acc = []) length list =
  if length = 0 then
    List.rev acc
  else
    match list with
      | [] ->
          List.rev acc
      | head :: tail ->
          truncate_list ~acc: (head :: acc) (length - 1) tail
