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

let type_file_ast env file_ast =
  try
    Typing.check_file env file_ast
  with
    | Typing.Error (loc, message) ->
        error loc message;
        env, []

let run_typed_file state typed_file =
  Run.run_file state typed_file

let run_ast state env ast =
  let env, typed = type_file_ast env ast in
  run_typed_file state typed;
  env

let run_file state env filename =
  run_ast state env (parse_file filename)

let run_string state env string =
  run_ast state env (parse_string string)

let init () =
  let env = ref Typing.empty_env in
  let run_file state filename = env := run_file state !env filename in
  let run_string state string = env := run_string state !env string in
  let overload_command name typ value = env := Typing.overload_command name typ value !env in
  let overload_variable name typ value = env := Typing.overload_variable name typ value !env in
  run_file, run_string, overload_command, overload_variable
