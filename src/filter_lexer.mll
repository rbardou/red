rule items acc = parse
  | ' '+
    { items acc lexbuf }
  | [^' ']+ as item
    { items (item :: acc) lexbuf }
  | eof
    { List.rev acc }
