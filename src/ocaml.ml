module Lexer =
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

    | Char
    | Char_backslash
    | Char_char
    | Char_end

    | String
    | String_backslash
    | String_backslash_digit
    | String_backslash_digit_digit
    | String_backslash_invalid
    | String_backslash_end
    | String_end

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
      | Char -> "Char"
      | Char_backslash -> "Char_backslash"
      | Char_char -> "Char_char"
      | Char_end -> "Char_end"
      | String -> "String"
      | String_backslash -> "String_backslash"
      | String_backslash_digit -> "String_backslash_digit"
      | String_backslash_digit_digit -> "String_backslash_digit_digit"
      | String_backslash_invalid -> "String_backslash_invalid"
      | String_backslash_end -> "String_backslash_end"
      | String_end -> "String_end"

  let other = Style.make ~fg: Yellow ()
  let invalid = Style.make ~bg: Red ~fg: Black ()
  let keyword = Style.bold ~fg: Yellow ()
  let identifier = Style.default
  let constructor = Style.bold ()
  let module_ = Style.bold ~fg: Blue ()
  let integer = Style.make ~fg: Blue ()
  let float = Style.make ~fg: Cyan ()
  let comment = Style.make ~fg: Green ()
  let char = Style.make ~fg: Magenta ()
  let string = Style.make ~fg: Magenta ()
  let string_escaped = Style.make ~fg: Black ~bg: Magenta ()

  let style state =
    match state with
      | Other -> other
      | Invalid -> invalid
      | Identifier chars ->
          (
            match String.concat "" (List.map (String.make 1) (List.rev chars)) with
              | "let" | "in" | "match" | "with" | "if" | "then" | "else" | "begin" | "end" | "module" | "struct"
              | "sig" | "type" | "while" | "do" | "done" | "for" | "to" ->
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
      | Char -> invalid
      | Char_backslash -> invalid
      | Char_char -> invalid
      | Char_end -> char
      | String -> string
      | String_backslash -> invalid
      | String_backslash_digit -> invalid
      | String_backslash_digit_digit -> invalid
      | String_backslash_invalid -> invalid
      | String_backslash_end -> string_escaped
      | String_end -> string

  let start = Other

  let add_char (type a) (character: Character.t) (continue: t -> a) (start: Render.style -> t -> a) state: a =
    let start new_state = start (style state) new_state in
    if String.length character = 0 then
      continue state
    else
      match state, character.[0] with

        | Identifier chars, ('a'..'z' | 'A'..'Z' | '_' as char) ->
            continue (Identifier (char :: chars))
        | Uppercase_identifier chars, ('a'..'z' | 'A'..'Z' | '_' as char) ->
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
        | (Comment_star _ | Comment_parenthesis _), _ -> continue state

        | Char, '\\' -> continue Char_backslash
        | (Char | Char_backslash), _ -> continue Char_char
        | Char_char, '\'' -> continue Char_end
        | Char_char, _ -> continue Invalid

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
        | _, '\'' -> start Char
        | _, '"' -> start String

        | (Other | Invalid), _ -> continue Other
        | (Identifier _ | Sign | Integer | Float | Float_exponent_digits | Parenthesis
          | Comment_end | String_end | Char_end | Uppercase_identifier _ | Module), _ ->
            start Other

  let end_of_file (type a) state: Render.style =
    style state

end

let test verbose string =
  let set_style start index (style: Render.style) =
    Term.fg_color style.fg;
    Term.bg_color style.bg;
    Term.intensity style.intensity;
    if style.underline then Term.underline true;
    print_string (String.sub string start (index - start));
    Term.reset_style ()
  in
  let rec loop start index state =
    if verbose then print_endline ("State: " ^ Lexer.show state);
    if index >= String.length string then
      (
        set_style start index (Lexer.end_of_file state);
        if verbose then (
          print_newline ();
          print_endline ("Final state: " ^ Lexer.show state);
        );
      )
    else
      let continue state = loop start (index + 1) state in
      let start style state = set_style start index style; loop index (index + 1) state in
      if verbose then Printf.printf "Char: %C\n" string.[index];
      Lexer.add_char (String.make 1 string.[index]) continue start state
  in
  loop 0 0 Lexer.start

let () =
  test false "\
let x = 42 in \"pl\\nop\"
let char = 'a' or '\\''
let int = 42 + -12
(* floats can have various forms (* and comments can be nested *) *)
let float = -12. or 5.17 or 5.18e28 or 1_000.e10_000

for i = 0 to 42 do
  Printf.printf \"i = %d\" i
done;

match color with
  | Red -> \"Red\"
  | Green -> \"Green\"
  | Blue -> \"Blue\"

\"strings can contain escaped octal \\100 but they must \\10 be \\1 terminated
they can be multiline
and they can contain double quotes \\\" which do not end the string\"

(* comments must be terminated *) (* especially (* nested *) *
"
