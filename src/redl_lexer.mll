{
  open Redl_parser

  exception Error of string

  let error x =
    Printf.ksprintf (fun s -> raise (Error s)) x

  type context =
    | Beginning_of_line
    | Not_beginning_of_line

  type state =
    {
      buffer: Buffer.t;
      mutable context: context;

      (* Queue in reverse order: new tokens are added to the front.
         When popping the queue, one must reverse it first. *)
      mutable queue_rev: token list;

      (* Queue in regular order.
         Cannot add tokens to [queue_rev] until [queue] is empty. *)
      mutable queue: token list;

      (* Parentheses and braces increment this level.
         If it is greater than 0, ignore indentation. *)
      mutable parenthesis_level: int;

      (* Stack of indentation levels that we can dedent to.
         Empty if current indentation level is 0.
         Else, the first level is the current level. *)
      mutable indentation: int list;
    }

  let push state token =
    if state.queue <> [] then invalid_arg "Redl_lexer.push";
    state.queue_rev <- token :: state.queue_rev

  let next state =
    match state.queue with
      | head :: tail ->
          state.queue <- tail;
          Some head
      | [] ->
          match List.rev state.queue_rev with
            | head :: tail ->
                state.queue_rev <- [];
                state.queue <- tail;
                Some head
            | [] ->
                None

  let set_indentation state new_indentation =
    let current_indentation =
      match state.indentation with
        | [] ->
            0
        | head :: _ ->
            head
    in
    (* Indent. *)
    if new_indentation > current_indentation then
      (
        state.indentation <- new_indentation :: state.indentation;
        if state.parenthesis_level = 0 then push state LBRACE;
      )

    (* Dedent (may have several levels dedented at once). *)
    else if new_indentation < current_indentation then
      let rec pop_indentation state =
        match state.indentation with
          | [] ->
              if new_indentation <> 0 then
                error "invalid indentation"
          | head :: tail ->
              if head > new_indentation then
                (
                  state.indentation <- tail;
                  if state.parenthesis_level = 0 then push state RBRACE;
                  pop_indentation state;
                )
              else if head < new_indentation then
                error "invalid indentation"
      in
      pop_indentation state

  let keyword = function
    | "command" -> COMMAND
    | x -> IDENTIFIER x
}

let blank = [ ' ' '\t' '\r' ]
let digits = ['0'-'9'] ['0'-'9' '_']*
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let comment = '#' [^'\n']*

rule beginning_of_line state = parse
  (* Whitespace and comments in empty lines are ignored. *)
  | blank* comment? '\n'
      {
        Lexing.new_line lexbuf;
      }
  | blank* comment? eof
      {
        set_indentation state 0;
        push state EOF;
      }

  (* Otherwise, a line may start with a keyword or an indentifier. *)
  | (blank* as indentation) (identifier as identifier)
      {
        set_indentation state (String.length indentation);
        push state (keyword identifier);
        state.context <- Not_beginning_of_line;
      }

  (* Anything else is a parsing error. *)
  | _ as char
      { error "unexpected characters: %c%s" char (parse_more lexbuf) }

and parse_more = parse
  | _? _? _? _? _? _? _? _?
    _? _? _? _? _? _? _? _?
    _? _? _? _? _? _? _? _?
    _? _? _? _? _? _? _? _? as x
      { x }

and not_beginning_of_line state = parse
  (* At the end of a line we go back to the [Beginning_of_line] mode of parsing,
     unless we are in some parentheses. *)
  | comment? '\n'
      {
        Lexing.new_line lexbuf;
        if state.parenthesis_level = 0 then (
          state.context <- Beginning_of_line;
          push state SEMI;
        );
      }
  | comment? eof
      {
        push state SEMI;
        set_indentation state 0;
        state.context <- Beginning_of_line;
        push state EOF;
      }

  (* Ignore whitespace when we are not at the beginning of a new line. *)
  | blank+
      {}

  (* Strings require specific parsing rules. *)
  | '"'
      { string state lexbuf }

  (* Regular tokens. *)
  | identifier as x
      { push state (keyword x) }
  | '-'? digits ('.' digits? ('e' ['+' '-']? digits)? | 'e' ['+' '-']? digits) as x
      {
        let value =
          try
            float_of_string x
          with _ ->
            error "invalid float literal: %S" x
        in
        push state (FLOAT value)
      }
  | '-'? ['0'-'9'] ['0'-'9' '_']* as x
      {
        let value =
          try
            int_of_string x
          with _ ->
            error "invalid integer literal: %S" x
        in
        push state (INT value)
      }
  | '('
      {
        state.parenthesis_level <- state.parenthesis_level + 1;
        push state LPAR;
      }
  | ')'
      {
        state.parenthesis_level <- state.parenthesis_level - 1;
        push state RPAR;
      }
  | '{'
      {
        state.parenthesis_level <- state.parenthesis_level + 1;
        push state LBRACE;
      }
  | '}'
      {
        state.parenthesis_level <- state.parenthesis_level - 1;
        push state RBRACE;
      }
  | ':'
      { push state COLON }
  | ';'
      { push state SEMI }
  | '='
      { push state EQUAL }

  (* Anything else is a parsing error. *)
  | _ as char
      { error "unexpected characters: %c%s" char (parse_more lexbuf) }

and string state = parse
  | "\n"
    {
      Lexing.new_line lexbuf;
      Buffer.add_char state.buffer '\n';
      string state lexbuf
    }
  | "\\\n"
    { Lexing.new_line lexbuf; blank_then_string state lexbuf }
  | "\\\\"
    { Buffer.add_char state.buffer '\\'; string state lexbuf }
  | "\\\""
    { Buffer.add_char state.buffer '"'; string state lexbuf }
  | "\\r"
    { Buffer.add_char state.buffer '\r'; string state lexbuf }
  | "\\n"
    { Buffer.add_char state.buffer '\n'; string state lexbuf }
  | "\\t"
    { Buffer.add_char state.buffer '\t'; string state lexbuf }
  | "\\b"
    { Buffer.add_char state.buffer '\b'; string state lexbuf }
  | "\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] as x)
    {
      let char =
        try
          Char.chr (int_of_string x)
        with Invalid_argument _ ->
          error "invalid character: \\%s" x
      in
      Buffer.add_char state.buffer char; string state lexbuf }
  | "\\{"
    { Buffer.add_char state.buffer '{'; string state lexbuf }
  | "\\}"
    { Buffer.add_char state.buffer '}'; string state lexbuf }
  | "\\" _ as s
    { error "invalid character: %s" s }
  | '"'
    {
      let s = Buffer.contents state.buffer in
      Buffer.clear state.buffer;
      push state (STRING s)
    }
  | eof
    { error "string not terminated" }
  | _ as c
    { Buffer.add_char state.buffer c; string state lexbuf }

(* Not inlined because otherwise [new_line] would be called too late and locations would be wrong. *)
and blank_then_string state = parse
  | blank*
    { string state lexbuf }

{

  let start () =
    {
      buffer = Buffer.create 256;
      context = Beginning_of_line;
      queue = [];
      queue_rev = [];
      parenthesis_level = 0;
      indentation = [];
    }

  let show = function
    | INT _ -> "INT"
    | FLOAT _ -> "FLOAT"
    | IDENTIFIER _ -> "IDENTIFIER"
    | STRING _ -> "STRING"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | COLON -> "COLON"
    | SEMI -> "SEMI"
    | EQUAL -> "EQUAL"
    | COMMAND -> "COMMAND"
    | EOF -> "EOF"

  let rec token state lexbuf =
    match next state with
      | Some token ->
          (* print_endline (show token); *)
          token
      | None ->
          match state.context with
            | Beginning_of_line ->
                beginning_of_line state lexbuf;
                token state lexbuf
            | Not_beginning_of_line ->
                not_beginning_of_line state lexbuf;
                token state lexbuf

}
