type location = Lexing.position * Lexing.position

type 'a located =
  {
    loc: location;
    value: 'a;
  }

type identifier = string located

type expression =
  | Int of int
  | Float of float
  | String of string
  | Variable of string
  | Apply of expression located * expression located list

type statement =
  | Command of identifier * expression located list
  | Sequence of statement located * statement located

type command_definition =
  {
    name: identifier;
    parameters: (identifier * identifier) list; (* name, type *)
    body: statement located;
  }

type toplevel =
  | Command_definition of command_definition located
  | Statement of statement located

type file = toplevel list

let foreach l f = List.iter f l

(******************************************************************************)

let out_indent out (level: int) =
  out (String.make (level * 2) ' ')

let rec out_expression out (expression: expression) =
  match expression with
    | Int i ->
        out (string_of_int i)
    | Float f ->
        out (string_of_float f)
    | String s ->
        out "\""; out (String.escaped s); out "\""
    | Variable x ->
        out x
    | Apply (f, arguments) ->
        out "(";
        out_expression out f.value;
        (
          foreach arguments @@ fun argument ->
          out " ";
          out_expression out argument.value
        );
        out ")"

let rec out_statement level out (statement: statement) =
  match statement with
    | Command (name, arguments) ->
        out_indent out level;
        out name.value;
        (
          foreach arguments @@ fun argument ->
          out " "; out_expression out argument.value
        );
        out "\n"
    | Sequence (a, b) ->
        out_statement level out a.value;
        out_statement level out b.value

let out_command_name_and_parameters out name parameters =
  out "command "; out name.value;
  (
    foreach parameters @@ fun (name, typ) ->
    out " ("; out name.value; out ": "; out typ.value; out ")"
  );
  out " ="

let out_toplevel out (toplevel: toplevel) =
  match toplevel with
    | Command_definition { value = { name; parameters; body } } ->
        out_command_name_and_parameters out name parameters; out "\n";
        out_statement 1 out body.value
    | Statement statement ->
        out_statement 0 out statement.value

let out_file out (file: file) =
  foreach file (out_toplevel out)

(******************************************************************************)

let rec out_statement_flat braces_if_sequence out (statement: statement) =
  match statement with
    | Command (name, arguments) ->
        out name.value;
        (
          foreach arguments @@ fun argument ->
          out " "; out_expression out argument.value
        )
    | Sequence (a, b) ->
        if braces_if_sequence then out "{ ";
        out_statement_flat false out a.value;
        out "; ";
        out_statement_flat false out b.value;
        if braces_if_sequence then out " }"

let out_toplevel_flat out (toplevel: toplevel) =
  match toplevel with
    | Command_definition { value = { name; parameters; body } } ->
        out_command_name_and_parameters out name parameters; out " ";
        out_statement_flat true out body.value
    | Statement statement ->
        out_statement_flat false out statement.value

let out_file_flat out (file: file) =
  let first = ref true in
  foreach file @@ fun toplevel ->
  if !first then first := false else out "; ";
  out_toplevel_flat out toplevel

(******************************************************************************)

let rec statement_uses_command name (statement: statement) =
  match statement with
    | Command (n, _) ->
        name = n.value
    | Sequence (a, b) ->
        statement_uses_command name a.value ||
        statement_uses_command name b.value

let toplevel_uses_command name (toplevel: toplevel) =
  match toplevel with
    | Command_definition _ ->
        false
    | Statement statement ->
        statement_uses_command name statement.value

let file_uses_command name (file: file) =
  List.exists (toplevel_uses_command name) file
