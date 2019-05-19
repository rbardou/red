rule items acc current_item = parse
  | ' '+
    {
      if current_item = [] then
        items acc [] lexbuf
      else
        let current_item = String.concat "" (List.rev current_item) in
        items (current_item :: acc) [] lexbuf
    }

  | '\''
    { items acc (single_quote lexbuf :: current_item) lexbuf }

  | '\"'
    { items acc (double_quote current_item lexbuf) lexbuf }

  | '\\'
    { items acc (backslash lexbuf :: current_item) lexbuf }

  | [^' ' '\'' '"' '\\']+ as x
    { items acc (x :: current_item) lexbuf }

  | eof
    {
      if current_item = [] then
        List.rev acc
      else
        List.rev_append acc [ String.concat "" (List.rev current_item) ]
    }

and single_quote = parse
  | ([^'\'']* as x) '\''
    { x }
  | [^'\'']* eof
    { failwith "unmatched single quote" }

and double_quote current_item = parse
  | '"'
    { current_item }
  | '\\'
    { double_quote (backslash lexbuf :: current_item) lexbuf }
  | [^'\"' '\\']+ as x
    { double_quote (x :: current_item) lexbuf }
  | eof
    { failwith "unmatched single quote" }

and backslash = parse
  | ([^'\\'] as x)
    { String.make 1 x }
  | eof
    { failwith "end of file after backslash" }
