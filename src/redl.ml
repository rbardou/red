module Ast = Redl_ast
module Lexer = Redl_lexer
module Parser = Redl_parser
module Typing = Redl_typing
module Run = Redl_run

let error ((a, b): Ast.location) message =
  (* TODO: open file at location and display error there immediately? *)
  Log.error "File \"%s\", line %d, characters %d-%d: %s"
    a.pos_fname
    a.pos_lnum
    (a.pos_cnum - a.pos_bol)
    (b.pos_cnum - a.pos_bol)
    message

let parse_lexbuf lexbuf =
  try
    Parser.file (Lexer.token (Lexer.start ())) lexbuf
  with
    | Parsing.Parse_error ->
        error (lexbuf.lex_start_p, lexbuf.lex_curr_p) "parse error";
        []
    | Lexer.Error message ->
        error (lexbuf.lex_start_p, lexbuf.lex_curr_p) ("parse error: " ^ message);
        []

let parse_file filename =
  System.with_open_in filename @@ fun ch ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_lexbuf lexbuf

let parse_string string =
  parse_lexbuf (Lexing.from_string string)

let type_check env file_ast =
  try
    Typing.check_file env file_ast
  with
    | Typing.Error (loc, message) ->
        error loc message;
        env, []

let run typed_file state =
  Run.run_file state typed_file

(* Return a [type_check] function which manages its own environment in its closure,
   and an [overload_command] function to enrich this environment. *)
let init () =
  let env = ref Typing.empty_env in
  let type_check file_ast =
    let new_env, typed_file = type_check !env file_ast in
    env := new_env;
    typed_file
  in
  let overload_command name partial = env := Typing.overload_command name partial !env in
  type_check, overload_command
