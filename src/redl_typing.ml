open Misc
module Ast = Redl_ast

exception Error of Ast.location * string

let error loc s = raise (Error (loc, s))
let error loc x = Printf.ksprintf (error loc) x

type command = State.t -> unit

(** Description of type ['f], which, if it is a function, eventually returns a value of type ['r]. *)
type ('f, 'r) typ =
  | Unit: (unit, unit) typ
  | Bool: (bool, bool) typ
  | Int: (int, int) typ
  | Float: (float, float) typ
  | String: (string, string) typ
  | Command: (command, command) typ
  | Function: ('a, 'a) typ * ('b, 'c) typ -> ('a -> 'b, 'c) typ

type 'a expression =
  | Constant of 'a
  | Sequence: command expression * command expression -> command expression
  | Variable: ('a, _) typ * string -> 'a expression (* need variable type to add it to runtime env *)
  | Lambda: ('a, _) typ * string * 'b expression -> ('a -> 'b) expression (* same here *)
  | Apply: ('a -> 'b) expression * 'a expression -> 'b expression

(** A value of type ['f], which, if it is a function, eventually returns a value of type ['r]. *)
type ('f, 'r) typed =
  {
    typ: ('f, 'r) typ;
    value: 'f expression;
  }

(** An expression packed with its type, which eventually returns a value of type ['r]. *)
type 'r partial = Partial: ('f, 'r) typed -> 'r partial

(** An expression packed with its type. *)
type packed = Packed: ('f, 'r) typed -> packed

(** An expression packed with its type, losing the precision on the return type. *)
type packed_type = Type: ('r, 'r) typ -> packed_type

(** A list of expressions which all return the same type. *)
type packed_list = List: ('r, 'r) typ * 'r partial list -> packed_list

type toplevel =
  | Command_definition: command partial -> toplevel
  | Statement of command expression

type file = toplevel list

(** Map from name to definitions for this name.
    All definitions for a given name return the same type, but parameter types may differ. *)
type env =
  {
    types: packed_type String_map.t;
    commands: command partial list String_map.t;
    variables: packed_list String_map.t;
  }

let empty_env =
  {
    types =
      String_map.empty
      |> String_map.add "unit" (Type Unit)
      |> String_map.add "bool" (Type Bool)
      |> String_map.add "int" (Type Int)
      |> String_map.add "float" (Type Float)
      |> String_map.add "string" (Type String)
      |> String_map.add "command" (Type Command);
    commands = String_map.empty;
    variables =
      String_map.empty
      |> String_map.add "true" (List (Bool, [ Partial { typ = Bool; value = Constant true } ]))
      |> String_map.add "false" (List (Bool, [ Partial { typ = Bool; value = Constant false } ]));
  }

let rec get_return_type: type f r. (f, r) typ -> (r, r) typ = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Float -> Float
  | String -> String
  | Command -> Command
  | Function (_, r) -> get_return_type r

type (_, _) type_equality =
  | Yes: ('a, 'a) type_equality
  | No: ('a, 'b) type_equality

let rec show_type: type f r. bool -> (f, r) typ -> string = fun parentheses -> function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Command -> "command"
  | Function (a, b) ->
      if parentheses then
        "(" ^ show_type true a ^ " -> " ^ show_type false b ^ ")"
      else
        show_type true a ^ " -> " ^ show_type false b

let rec dummy: type f r. (f, r) typ -> f = function
  | Unit -> ()
  | Bool -> false
  | Int -> 0
  | Float -> 0.
  | String -> ""
  | Command -> (fun _ -> ())
  | Function (a, b) -> (fun _ -> dummy b)

let rec equal_types: type a b c d. (a, c) typ -> (b, d) typ -> (a, b) type_equality = fun a b ->
  match a, b with
    | Unit, Unit -> Yes
    | Unit, _ -> No
    | Bool, Bool -> Yes
    | Bool, _ -> No
    | Int, Int -> Yes
    | Int, _ -> No
    | Float, Float -> Yes
    | Float, _ -> No
    | String, String -> Yes
    | String, _ -> No
    | Command, Command -> Yes
    | Command, _ -> No
    | Function (a1, b1), Function (a2, b2) ->
        (
          match equal_types a1 a2 with
            | No -> No
            | Yes ->
                match equal_types b1 b2 with
                  | No -> No
                  | Yes -> Yes
        )
    | Function _, _ ->
        No

let overload_command (type f) (name: string) (definition: command partial) (env: env): env =
  let existing =
    match String_map.find name env.commands with
      | exception Not_found -> []
      | l -> l
  in
  { env with commands = String_map.add name (definition :: existing) env.commands }

let overload_variable (type r) (name: string) (value: r partial) (env: env): env =
  let Partial value = value in
  let return_type = get_return_type value.typ in
  match String_map.find name env.variables with
    | exception Not_found ->
        { env with variables = String_map.add name (List (return_type, [ Partial value ])) env.variables }
    | List (list_type, existing) ->
        match equal_types list_type return_type with
          | Yes ->
              { env with variables = String_map.add name (List (return_type, Partial value :: existing)) env.variables }
          | No ->
              invalid_arg "overload_partial_variable: new value returns a different type"

(* Check whether [definition] is a function that can take [arguments] to return a [r].
   If so, evaluate to return [r] as a singleton list.
   Else, return the empty list. *)
let rec check_instance: type r. packed list -> r partial -> r expression list = fun arguments definition ->
  let Partial definition = definition in
  match arguments, definition.typ with
    | [], Unit ->
        [ definition.value ]
    | [], Bool ->
        [ definition.value ]
    | [], Int ->
        [ definition.value ]
    | [], Float ->
        [ definition.value ]
    | [], String ->
        [ definition.value ]
    | [], Command ->
        [ definition.value ]
    | [], Function _ ->
        [] (* missing argument *)
    | _ :: _, Unit ->
        [] (* too many arguments *)
    | _ :: _, Bool ->
        [] (* too many arguments *)
    | _ :: _, Int ->
        [] (* too many arguments *)
    | _ :: _, Float ->
        [] (* too many arguments *)
    | _ :: _, String ->
        [] (* too many arguments *)
    | _ :: _, Command ->
        [] (* too many arguments *)
    | Packed head :: tail, Function (parameter_type, result_type) ->
        match equal_types head.typ parameter_type with
          | No ->
              [] (* argument has the wrong type *)
          | Yes ->
              check_instance tail (Partial { typ = result_type; value = Apply (definition.value, head.value) })

let rec check_expression (env: env) (expression: Ast.expression Ast.located): packed =
  match expression.value with
    | Int i ->
        Packed { typ = Int; value = Constant i }
    | Float f ->
        Packed { typ = Float; value = Constant f }
    | String s ->
        Packed { typ = String; value = Constant s }

    | Variable name ->
        (
          match String_map.find name env.variables with
            | exception Not_found ->
                error expression.loc "unknown variable: %s" name
            | List (_, []) ->
                error expression.loc "unknown variable: %s" name
            | List (_, [ Partial value ]) ->
                Packed value
            | List (_, _ :: _ :: _) ->
                error expression.loc "ambiguous usage of %s" name
        )

    | Apply ({ value = Variable name; loc = name_loc }, arguments) ->
        let definitions =
          match String_map.find name env.variables with
            | exception Not_found ->
                error name_loc "unknown variable: %s" name
            | l ->
                l
        in
        let List (return_type, definitions) = definitions in
        let arguments = List.map (check_expression env) arguments in
        let valid_instances = List.map (check_instance arguments) definitions |> List.flatten in
        let instance =
          match valid_instances with
            | [] ->
                error expression.loc "cannot call %s with these arguments" name
            | [ instance ] ->
                instance
            | _ :: _ :: _ ->
                error expression.loc "ambiguous call to %s" name
        in
        Packed { typ = return_type; value = instance }

    | Apply (f, arguments) ->
        let f = check_expression env f in
        let rec check_arguments (f: packed) (arguments: Ast.expression Ast.located list) =
          match arguments with
            | [] ->
                f
            | head :: tail ->
                let Packed f = f in
                match f.typ with
                  | Unit ->
                      error expression.loc "unit is not a function"
                  | Bool ->
                      error expression.loc "booleans are not functions"
                  | Int ->
                      error expression.loc "integers are not functions"
                  | Float ->
                      error expression.loc "floats are not functions"
                  | String ->
                      error expression.loc "strings are not functions"
                  | Command ->
                      error expression.loc "commands are not functions"
                  | Function (arg_type, ret_type) ->
                      let head_loc = head.loc in
                      let Packed head = check_expression env head in
                      match equal_types arg_type head.typ with
                        | No ->
                            error head_loc "this argument must have type %s" (show_type false arg_type)
                        | Yes ->
                            check_arguments (Packed { typ = ret_type; value = Apply (f.value, head.value) }) tail
        in
        check_arguments f arguments

let rec check_statement (env: env) (statement: Ast.statement Ast.located): command expression =
  match statement.value with
    | Command (name, arguments) ->
        let definitions =
          match String_map.find name.value env.commands with
            | exception Not_found ->
                error name.loc "unknown command: %s" name.value
            | l ->
                l
        in
        let arguments = List.map (check_expression env) arguments in
        let valid_instances = List.map (check_instance arguments) definitions |> List.flatten in
        let instance =
          match valid_instances with
            | [] ->
                error statement.loc "cannot call %s with these arguments" name.value
            | [ instance ] ->
                instance
            | _ :: _ :: _ ->
                error statement.loc "ambiguous call to %s" name.value
        in
        instance
    | Sequence (a, b) ->
        let a = check_statement env a in
        let b = check_statement env b in
        Sequence (a, b)

let check_type_expression (env: env) (type_name: Ast.identifier): packed_type =
  match String_map.find type_name.value env.types with
    | exception Not_found ->
        error type_name.loc "unknown type: %s" type_name.value
    | typ ->
        typ

let check_command_definition (env: env) (definition: Ast.command_definition Ast.located): command partial =
  let parameters = definition.value.parameters in
  let body = definition.value.body in
  let rec check_parameters env (parameters: (Ast.identifier * Ast.identifier) list) =
    match parameters with
      | [] ->
          Partial { typ = Command; value = check_statement env body }
      | (parameter_name, parameter_type) :: tail ->
          let parameter_type = check_type_expression env parameter_type in
          let Type parameter_type = parameter_type in
          let env =
            let variables =
              String_map.add parameter_name.value (
                List (
                  parameter_type,
                  (* When we'll check the expression, we'll get a Variable to resolve later. *)
                  [ Partial { typ = parameter_type; value = (Variable (parameter_type, parameter_name.value)) } ]
                )
              ) env.variables
            in
            { env with variables }
          in
          let Partial { typ = result_type; value = result_expression } = check_parameters env tail in
          Partial {
            typ = Function (parameter_type, result_type);
            value = Lambda (parameter_type, parameter_name.value, result_expression);
          }
  in
  check_parameters env parameters

let check_toplevel (env: env) (toplevel: Ast.toplevel): env * toplevel =
  match toplevel with
    | Command_definition definition ->
        let name = definition.value.name.value in
        let definition = check_command_definition env definition in
        let env = overload_command name definition env in
        env, Command_definition definition
    | Statement statement ->
        let statement = check_statement env statement in
        env, Statement statement

let check_file env file =
  let rec check_file (env: env) (acc: file) (file: Ast.file): env * file =
    match file with
      | [] ->
          env, List.rev acc
      | head :: tail ->
          let env, toplevel = check_toplevel env head in
          check_file env (toplevel :: acc) tail
  in
  check_file env [] file
