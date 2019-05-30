module Stylist =
struct

  type t =
    | Other
    | Invalid

    | Identifier of char list (* in reverse order *)
    | Uppercase_identifier of char list (* in reverse order *)
    | Module

    | Sign
    | Integer
    | Float
    | Float_exponent
    | Float_exponent_digits

    | Parenthesis
    | Comment of int
    | Comment_star of int
    | Comment_parenthesis of int
    | Comment_end

    | Quote
    | Char_backslash
    | Quote_type_variable_char
    | Char_char
    | Char_end
    | Type_variable

    | String
    | String_backslash
    | String_backslash_digit
    | String_backslash_digit_digit
    | String_backslash_invalid
    | String_backslash_end
    | String_end

  let equivalent = (=)

  let start = Other

  let show state =
    match state with
      | Other -> "Other"
      | Invalid -> "Invalid"
      | Identifier _ -> "Identifier"
      | Uppercase_identifier _ -> "Uppercase_identifier"
      | Module -> "Module"
      | Sign -> "Sign"
      | Integer -> "Integer"
      | Float -> "Float"
      | Float_exponent -> "Float_exponent"
      | Float_exponent_digits -> "Float_exponent_digits"
      | Parenthesis -> "Parenthesis"
      | Comment n -> "Comment " ^ string_of_int n
      | Comment_star n -> "Comment_star " ^ string_of_int n
      | Comment_parenthesis n -> "Comment_parenthesis " ^ string_of_int n
      | Comment_end -> "Comment_end"
      | Quote -> "Quote"
      | Char_backslash -> "Char_backslash"
      | Quote_type_variable_char -> "Quote_type_variable_char"
      | Char_char -> "Char_char"
      | Char_end -> "Char_end"
      | Type_variable -> "Type_variable"
      | String -> "String"
      | String_backslash -> "String_backslash"
      | String_backslash_digit -> "String_backslash_digit"
      | String_backslash_digit_digit -> "String_backslash_digit_digit"
      | String_backslash_invalid -> "String_backslash_invalid"
      | String_backslash_end -> "String_backslash_end"
      | String_end -> "String_end"

  let style state =
    let open Common_style in
    match state with
      | Other -> other
      | Invalid -> invalid
      | Identifier chars ->
          (
            match String.concat "" (List.map (String.make 1) (List.rev chars)) with
              | "and" | "as" | "assert" | "begin" | "class" | "constraint" | "do" | "done" | "downto"
              | "else" | "end" | "exception" | "external" | "false" | "for" | "fun" | "function" | "functor"
              | "if" | "in" | "include" | "inherit" | "initializer" | "lazy" | "let" | "match" | "method"
              | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or" | "private"
              | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type" | "val" | "virtual"
              | "when" | "while" | "with" | "lor" | "lxor" | "mod" | "land" | "lsl" | "lsr" | "asr" ->
                  keyword
              | _ ->
                  identifier
          )
      | Uppercase_identifier chars -> constructor
      | Module -> module_
      | Sign -> other
      | Integer -> integer
      | Float -> float
      | Float_exponent -> invalid
      | Float_exponent_digits -> float
      | Parenthesis -> other
      | Comment _ -> invalid
      | Comment_star _ -> invalid
      | Comment_parenthesis _ -> invalid
      | Comment_end -> comment
      | Quote -> invalid
      | Char_backslash -> invalid
      | Quote_type_variable_char -> type_variable
      | Char_char -> invalid
      | Char_end -> char
      | Type_variable -> type_variable
      | String -> string
      | String_backslash -> invalid
      | String_backslash_digit -> invalid
      | String_backslash_digit_digit -> invalid
      | String_backslash_invalid -> invalid
      | String_backslash_end -> string_escaped
      | String_end -> string

  let add_char (type a) (character: Character.t) state (continue: t -> a) (start: Style.t -> t -> a): a =
    let start new_state = start (style state) new_state in
    if String.length character = 0 then
      continue state
    else
      match state, character.[0] with

        | Identifier chars, ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as char) ->
            continue (Identifier (char :: chars))
        | Uppercase_identifier chars, ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as char) ->
            continue (Uppercase_identifier (char :: chars))
        | Uppercase_identifier chars, '.' ->
            continue Module

        | Sign, '0'..'9' -> continue Integer
        | Integer, ('0'..'9' | '_') -> continue Integer
        | Integer, '.' -> continue Float
        | Float, ('0'..'9' | '_') -> continue Float
        | Float, 'e' -> continue Float_exponent
        | Float_exponent, '0'..'9' -> continue Float_exponent_digits
        | Float_exponent, _ -> continue Invalid
        | Float_exponent_digits, ('0'..'9' | '_') -> continue Float_exponent_digits

        | Parenthesis, '*' -> continue (Comment 0)
        | (Comment depth | Comment_star depth), '*' -> continue (Comment_star depth)
        | (Comment depth | Comment_star depth | Comment_parenthesis depth), '(' -> continue (Comment_parenthesis depth)
        | Comment depth, _ -> continue (Comment depth)
        | Comment_parenthesis depth, '*' -> continue (Comment (depth + 1))
        | Comment_star 0, ')' -> continue Comment_end
        | Comment_star depth, ')' -> continue (Comment (depth - 1))
        | (Comment_star depth | Comment_parenthesis depth), _ -> continue (Comment depth)

        | Quote, '\\' -> continue Char_backslash
        | Quote, ('a'..'z' | 'A'..'Z') -> continue Quote_type_variable_char
        | Quote, _ -> continue Char_char
        | (Quote_type_variable_char | Char_char), '\'' -> continue Char_end
        | Char_char, _ -> continue Invalid
        | Char_backslash, _ -> continue Char_char
        | (Quote_type_variable_char | Type_variable), ('a'..'z' | 'A'..'Z' | '_') -> continue Type_variable

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

        | _, ('a'..'z' | '_' as char) -> start (Identifier [ char ])
        | _, ('A'..'Z' as char) -> start (Uppercase_identifier [ char ])
        | _, ('-' | '+') -> start Sign
        | _, ('0'..'9') -> start Integer
        | _, '(' -> start Parenthesis
        | _, '\'' -> start Quote
        | _, '"' -> start String

        | (Other | Invalid), _ -> continue Other
        | (Identifier _ | Sign | Integer | Float | Float_exponent_digits | Parenthesis
          | Comment_end | String_end | Char_end | Uppercase_identifier _ | Module | Quote_type_variable_char
          | Type_variable), _ ->
            start Other

  let end_of_file (type a) state: Style.t =
    style state

end

(* let test verbose string = *)
(*   let set_style start index (style: Style.t) = *)
(*     Term.fg_color style.fg; *)
(*     Term.bg_color style.bg; *)
(*     Term.intensity style.intensity; *)
(*     if style.underline then Term.underline true; *)
(*     print_string (String.sub string start (index - start)); *)
(*     Term.reset_style () *)
(*   in *)
(*   let rec loop start index state = *)
(*     if verbose then print_endline ("State: " ^ Stylist.show state); *)
(*     if index >= String.length string then *)
(*       ( *)
(*         set_style start index (Stylist.end_of_file state); *)
(*         if verbose then ( *)
(*           print_newline (); *)
(*           print_endline ("Final state: " ^ Stylist.show state); *)
(*         ); *)
(*       ) *)
(*     else *)
(*       let continue state = loop start (index + 1) state in *)
(*       let start style state = set_style start index style; loop index (index + 1) state in *)
(*       if verbose then Printf.printf "Char: %C\n" string.[index]; *)
(*       Stylist.add_char (String.make 1 string.[index]) state continue start *)
(*   in *)
(*   loop 0 0 Stylist.start *)

(* let () = *)
(*   test false "\ *)
(* let x = 42 in \"pl\\nop\" *)
(* let char = 'a' or '\\'' *)
(* let int = 42 + -12 *)
(* (\* floats can have various forms (\* and comments can be nested *\) *\) *)
(* let float = -12. or 5.17 or 5.18e28 or 1_000.e10_000 *)

(* for i = 0 to 42 do *)
(*   Printf.printf \"i = %d\" i *)
(* done; *)

(* match color with *)
(*   | Red -> \"Red\" *)
(*   | Green -> \"Green\" *)
(*   | Blue -> \"Blue\" *)

(* \"strings can contain escaped octal \\100 but they must \\10 be \\1 terminated *)
(* they can be multiline *)
(* and they can contain double quotes \\\" which do not end the string\" *)

(* (\* comments must be terminated *\) (\* especially (\* nested *\) * *)
(* " *)
