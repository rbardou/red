type 'state add_char_result =
  | Continue of 'state
  | End_token of {
      token_end_x: int; (* first character which is not in the token *)
      token_end_y: int;
      state: 'state; (* state to start reading the new token at [token_end_{x,y}] *)
      style: Style.t;
    }

type 'state t =
  {
    (* Initial state, i.e. the state before reading position [(0, 0)]. *)
    start: 'state;

    (* Test whether two states are similar enough to stop parsing.
       Should most often be [(=)]. *)
    equivalent: 'state -> 'state -> bool;

    (* Update the state of a parser after reading one character.

       Usage: [add_char character state x y]

       [x, y] is the position of [character]. *)
    add_char: Character.t -> 'state -> int -> int -> 'state add_char_result;

    (* Get the color of the final token. *)
    end_of_file: 'state -> Style.t;
  }

type packed = E: 'a t -> packed
