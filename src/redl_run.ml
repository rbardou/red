open Misc
module Typing = Redl_typing

type ('f, 'r) value =
  {
    typ: ('f, 'r) Typing.typ;
    value: 'f;
  }

type packed = Value: ('f, 'r) value -> packed

type env = packed String_map.t

let empty_env = String_map.empty

let rec eval: type a. State.t -> env -> a Typing.expression -> a =
  fun state env expression ->
    match expression with
      | Constant x ->
          x
      | Typing.Sequence (a, b) ->
          (
            fun state ->
              eval state env a state;
              eval state env b state
          )
      | Typing.Variable (typ, x) ->
          (
            match String_map.find x env with
              | exception Not_found ->
                  invalid_arg ("variable " ^ x ^ " is not in runtime environment, typing bug?")
              | Value value ->
                  match Typing.equal_types typ value.typ with
                    | Typing.Yes ->
                        value.value
                    | Typing.No ->
                        invalid_arg ("variable " ^ x ^ " has the wrong type in runtime environment, typing bug?")
          )
      | Typing.Lambda (arg_type, x, e) ->
          (
            fun var ->
              let env = String_map.add x (Value { typ = arg_type; value = var }) env in
              eval state env e
          )
      | Typing.Apply (f, arg) ->
          let f = eval state env f in
          let arg = eval state env arg in
          f arg

let run_toplevel (state: State.t) (toplevel: Typing.toplevel) =
  match toplevel with
    | Typing.Statement command ->
        eval state empty_env command state
    | Typing.Command_definition _ ->
        ()

let run_file (state: State.t) (file: Typing.file) =
  List.iter (run_toplevel state) file
