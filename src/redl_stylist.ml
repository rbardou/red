type t =
  | Other
  | Invalid

  | Identifier of char list (* in reverse order *)

  | Sign
  | Integer
  | Float
  | Float_exponent
  | Float_exponent_digits

  | String
  | String_backslash
  | String_backslash_digit
  | String_backslash_digit_digit
  | String_backslash_invalid
  | String_backslash_end
  | String_end

  | Comment
  | Comment_end

let equivalent = (=)

let start = Other

let style state =
  let open Common_style in
  match state with
    | Other -> other
    | Invalid -> invalid
    | Identifier chars ->
        (
          match String.concat "" (List.map (String.make 1) (List.rev chars)) with
            | "command" ->
                keyword
            | _ ->
                identifier
        )
    | Sign -> other
    | Integer -> integer
    | Float -> float
    | Float_exponent -> invalid
    | Float_exponent_digits -> float
    | String -> string
    | String_backslash -> invalid
    | String_backslash_digit -> invalid
    | String_backslash_digit_digit -> invalid
    | String_backslash_invalid -> invalid
    | String_backslash_end -> string_escaped
    | String_end -> string
    | Comment -> comment
    | Comment_end -> comment

let add_char (type a) (character: Character.t) state (continue: t -> a) (start: Style.t -> t -> a): a =
  let start new_state = start (style state) new_state in
  if String.length character = 0 then
    continue state
  else
    match state, character.[0] with

      | Identifier chars, ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as char) ->
          continue (Identifier (char :: chars))

      | Sign, '0'..'9' -> continue Integer
      | Integer, ('0'..'9' | '_') -> continue Integer
      | Integer, '.' -> continue Float
      | Float, ('0'..'9' | '_') -> continue Float
      | Float, 'e' -> continue Float_exponent
      | Float_exponent, '0'..'9' -> continue Float_exponent_digits
      | Float_exponent, _ -> continue Invalid
      | Float_exponent_digits, ('0'..'9' | '_') -> continue Float_exponent_digits

      | String, '"' -> continue String_end
      | String_backslash_end, '"' -> start String_end
      | (String | String_backslash_end), '\\' -> start String_backslash
      | String, _ -> continue String
      | (String_backslash_end | String_backslash_invalid), _ -> start String
      | String_backslash, '0'..'9' -> continue String_backslash_digit
      | String_backslash, _ -> continue String_backslash_end
      | String_backslash_digit, '0'..'9' -> continue String_backslash_digit_digit
      | String_backslash_digit, _ -> start String_backslash_invalid
      | String_backslash_digit_digit, '0'..'9' -> continue String_backslash_end
      | String_backslash_digit_digit, _ -> continue String_backslash_invalid

      | Comment, '\n' -> continue Comment_end
      | Comment, _ -> continue Comment

      | _, ('a'..'z' | 'A'..'Z' | '_' as char) -> start (Identifier [ char ])
      | _, ('-' | '+') -> start Sign
      | _, ('0'..'9') -> start Integer
      | _, '"' -> start String
      | _, '#' -> start Comment

      | (Other | Invalid), _ -> continue Other
      | (Identifier _ | Sign | Integer | Float | Float_exponent_digits | Comment_end | String_end), _ ->
          start Other

let end_of_file (type a) state: Style.t =
  style state
