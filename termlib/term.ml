(* Missing entry from module [Sys]: signal SIGWINCH, which indicates that
   window changed. *)
let sigwinch = 28

let show_cursor () =
  print_string "\027[?25h"

let reset_style () =
  print_string "\027[0m"

let clear () =
  print_string "\027[2J"

let goto_xy x y =
  print_string "\027[";
  print_int y;
  print_char ';';
  print_int x;
  print_char 'H'

(******************************************************************************)
(*                                  Raw Mode                                  *)
(******************************************************************************)

let original_terminal_io = ref None
let resize_signal_set = ref false
let size_changed = ref true
let cursor_is_visible = ref true

let restore () =
  (
    match !original_terminal_io with
      | None ->
          ()
      | Some terminal_io ->
          Unix.tcsetattr Unix.stdout TCSAFLUSH terminal_io
  );
  show_cursor ();
  reset_style ();
  goto_xy 0 0;
  clear ()

let raw_mode ?(restore_at_exit = true) () =
  match Unix.tcgetattr Unix.stdout with
    | exception Unix.Unix_error (Unix.ENOTTY, _, _) ->
        ()
    | terminal_io ->
        (* Save for future restore. *)
        (
          match !original_terminal_io with
            | None ->
                original_terminal_io := Some terminal_io
            | Some _ ->
                ()
        );

        (* Set mode. *)
        Unix.tcsetattr Unix.stdout TCSAFLUSH {
          terminal_io with
            (* Don't print what the user types. *)
            c_echo = false;

            (* No line editing: send each character immediately. *)
            c_icanon = false;

            (* Disable Ctrl+C and Ctrl+Z signals. *)
            c_isig = false;

            (* Disable Ctrl+S and Ctrl+Q. *)
            c_ixon = false;

            (* Don't convert \r to \n (fixes Ctrl+M). *)
            c_icrnl = false;

            (* Don't convert \n to \r\n. *)
            c_opost = false;

            (* Probably no effect on modern terminal. *)
            c_brkint = false;
            c_inpck = false;
            c_istrip = false;

            (* TODO: Not in current version of OCaml. *)
            (* c_iexten = false; (\* Disable Ctrl+V and sometimes Ctrl+O. *\) *)
        };

        (* Capture signal which indicates that terminal size changed. *)
        if not !resize_signal_set then (
          let handler _ = size_changed := true in
          Sys.set_signal sigwinch (Signal_handle handler);
          resize_signal_set := true
        );

        (* Restore at exit. *)
        if restore_at_exit then at_exit restore

let size_changed () =
  let result = !size_changed in
  size_changed := false;
  result

(******************************************************************************)
(*                                    Input                                   *)
(******************************************************************************)

(* For [Unicode] and [Escape_unicode]: char list is the list, in reverse
   order, of bytes already read, and int is the remaining number of bytes
   to read. *)
type input_state =
  | Empty
  | Unicode of char list * int
  | Escape
  | Escape_unicode of char list * int
  | Escape_bracket of int
  | Escape_bracket_semi of int * int
  | Escape_O of int
  | Escape_O_semi of int * int
  | Done of Key.t
  | Cursor_position of int * int (** column, row *)
  | Invalid of input_state * char

let rec show_input_state state =
  match state with
    | Empty ->
        "Empty"
    | Unicode (partial, remaining) ->
        Printf.sprintf "Unicode (%S, %d)"
          (Misc.string_of_char_list_rev partial) remaining
    | Escape ->
        "Escape"
    | Escape_unicode (partial, remaining) ->
        Printf.sprintf "Escape_unicode (%S, %d)"
          (Misc.string_of_char_list_rev partial) remaining
    | Escape_bracket x ->
        "Escape_bracket " ^ string_of_int x
    | Escape_bracket_semi (x, y) ->
        "Escape_bracket_semi (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
    | Escape_O x ->
        "Escape_O " ^ string_of_int x
    | Escape_O_semi (x, y) ->
        "Escape_O_semi (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
    | Done key ->
        "Done " ^ Key.show key
    | Cursor_position (x, y) ->
        "Cursor_position (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
    | Invalid (state, char) ->
        Printf.sprintf "Invalid (%s, %C)" (show_input_state state) char

let input_char state char: input_state =
  match state, char with
    (* 000 to 127. *)
    | Empty, '\000' -> Done Ctrl_arobase
    | Empty, '\001' -> Done Ctrl_a
    | Empty, '\002' -> Done Ctrl_b
    | Empty, '\003' -> Done Ctrl_c
    | Empty, '\004' -> Done Ctrl_d
    | Empty, '\005' -> Done Ctrl_e
    | Empty, '\006' -> Done Ctrl_f
    | Empty, '\007' -> Done Ctrl_g
    | Empty, '\008' -> Done Ctrl_h
    | Empty, '\009' -> Done Tab
    | Empty, '\010' -> Done Ctrl_j
    | Empty, '\011' -> Done Ctrl_k
    | Empty, '\012' -> Done Ctrl_l
    | Empty, '\013' -> Done Return
    | Empty, '\014' -> Done Ctrl_n
    | Empty, '\015' -> Done Ctrl_o
    | Empty, '\016' -> Done Ctrl_p
    | Empty, '\017' -> Done Ctrl_q
    | Empty, '\018' -> Done Ctrl_r
    | Empty, '\019' -> Done Ctrl_s
    | Empty, '\020' -> Done Ctrl_t
    | Empty, '\021' -> Done Ctrl_u
    | Empty, '\022' -> Done Ctrl_v
    | Empty, '\023' -> Done Ctrl_w
    | Empty, '\024' -> Done Ctrl_x
    | Empty, '\025' -> Done Ctrl_y
    | Empty, '\026' -> Done Ctrl_z
    | Empty, '\027' -> Escape
    | Empty, '\028' -> Done Ctrl_antislash
    | Empty, '\029' -> Done Ctrl_right_bracket
    | Empty, '\030' -> Done Ctrl_caret
    | Empty, '\031' -> Done Ctrl_underscore

    | Empty, ' ' -> Done Space
    | Empty, '!' -> Done Exclamation_mark
    | Empty, '"' -> Done Double_quote
    | Empty, '#' -> Done Hash
    | Empty, '$' -> Done Dollar
    | Empty, '%' -> Done Percent
    | Empty, '&' -> Done Ampersand
    | Empty, '\'' -> Done Quote
    | Empty, '(' -> Done Left_parenthesis
    | Empty, ')' -> Done Right_parenthesis
    | Empty, '*' -> Done Star
    | Empty, '+' -> Done Plus
    | Empty, ',' -> Done Comma
    | Empty, '-' -> Done Minus
    | Empty, '.' -> Done Period
    | Empty, '/' -> Done Slash
    | Empty, '0' -> Done Digit_0
    | Empty, '1' -> Done Digit_1
    | Empty, '2' -> Done Digit_2
    | Empty, '3' -> Done Digit_3
    | Empty, '4' -> Done Digit_4
    | Empty, '5' -> Done Digit_5
    | Empty, '6' -> Done Digit_6
    | Empty, '7' -> Done Digit_7
    | Empty, '8' -> Done Digit_8
    | Empty, '9' -> Done Digit_9
    | Empty, ':' -> Done Colon
    | Empty, ';' -> Done Semicolon
    | Empty, '<' -> Done Left_chevron
    | Empty, '=' -> Done Equal
    | Empty, '>' -> Done Right_chevron
    | Empty, '?' -> Done Question_mark
    | Empty, '@' -> Done Arobase

    | Empty, 'A' -> Done Letter_A
    | Empty, 'B' -> Done Letter_B
    | Empty, 'C' -> Done Letter_C
    | Empty, 'D' -> Done Letter_D
    | Empty, 'E' -> Done Letter_E
    | Empty, 'F' -> Done Letter_F
    | Empty, 'G' -> Done Letter_G
    | Empty, 'H' -> Done Letter_H
    | Empty, 'I' -> Done Letter_I
    | Empty, 'J' -> Done Letter_J
    | Empty, 'K' -> Done Letter_K
    | Empty, 'L' -> Done Letter_L
    | Empty, 'M' -> Done Letter_M
    | Empty, 'N' -> Done Letter_N
    | Empty, 'O' -> Done Letter_O
    | Empty, 'P' -> Done Letter_P
    | Empty, 'Q' -> Done Letter_Q
    | Empty, 'R' -> Done Letter_R
    | Empty, 'S' -> Done Letter_S
    | Empty, 'T' -> Done Letter_T
    | Empty, 'U' -> Done Letter_U
    | Empty, 'V' -> Done Letter_V
    | Empty, 'W' -> Done Letter_W
    | Empty, 'X' -> Done Letter_X
    | Empty, 'Y' -> Done Letter_Y
    | Empty, 'Z' -> Done Letter_Z

    | Empty, '[' -> Done Left_bracket
    | Empty, '\\' -> Done Antislash
    | Empty, ']' -> Done Right_bracket
    | Empty, '^' -> Done Caret
    | Empty, '_' -> Done Underscore
    | Empty, '`' -> Done Backquote

    | Empty, 'a' -> Done Letter_a
    | Empty, 'b' -> Done Letter_b
    | Empty, 'c' -> Done Letter_c
    | Empty, 'd' -> Done Letter_d
    | Empty, 'e' -> Done Letter_e
    | Empty, 'f' -> Done Letter_f
    | Empty, 'g' -> Done Letter_g
    | Empty, 'h' -> Done Letter_h
    | Empty, 'i' -> Done Letter_i
    | Empty, 'j' -> Done Letter_j
    | Empty, 'k' -> Done Letter_k
    | Empty, 'l' -> Done Letter_l
    | Empty, 'm' -> Done Letter_m
    | Empty, 'n' -> Done Letter_n
    | Empty, 'o' -> Done Letter_o
    | Empty, 'p' -> Done Letter_p
    | Empty, 'q' -> Done Letter_q
    | Empty, 'r' -> Done Letter_r
    | Empty, 's' -> Done Letter_s
    | Empty, 't' -> Done Letter_t
    | Empty, 'u' -> Done Letter_u
    | Empty, 'v' -> Done Letter_v
    | Empty, 'w' -> Done Letter_w
    | Empty, 'x' -> Done Letter_x
    | Empty, 'y' -> Done Letter_y
    | Empty, 'z' -> Done Letter_z

    | Empty, '{' -> Done Left_brace
    | Empty, '|' -> Done Pipe
    | Empty, '}' -> Done Right_brace
    | Empty, '~' -> Done Tilde
    | Empty, '\127' -> Done Backspace

    (* Escape followed by 000 to 127. *)
    | Escape, '\000' -> Done Alt_ctrl_arobase
    | Escape, '\001' -> Done Alt_ctrl_a
    | Escape, '\002' -> Done Alt_ctrl_b
    | Escape, '\003' -> Done Alt_ctrl_c
    | Escape, '\004' -> Done Alt_ctrl_d
    | Escape, '\005' -> Done Alt_ctrl_e
    | Escape, '\006' -> Done Alt_ctrl_f
    | Escape, '\007' -> Done Alt_ctrl_g
    | Escape, '\008' -> Done Alt_ctrl_h
    | Escape, '\009' -> Done Alt_tab
    | Escape, '\010' -> Done Alt_ctrl_j
    | Escape, '\011' -> Done Alt_ctrl_k
    | Escape, '\012' -> Done Alt_ctrl_l
    | Escape, '\013' -> Done Alt_ctrl_m
    | Escape, '\014' -> Done Alt_ctrl_n
    | Escape, '\015' -> Done Alt_ctrl_o
    | Escape, '\016' -> Done Alt_ctrl_p
    | Escape, '\017' -> Done Alt_ctrl_q
    | Escape, '\018' -> Done Alt_ctrl_r
    | Escape, '\019' -> Done Alt_ctrl_s
    | Escape, '\020' -> Done Alt_ctrl_t
    | Escape, '\021' -> Done Alt_ctrl_u
    | Escape, '\022' -> Done Alt_ctrl_v
    | Escape, '\023' -> Done Alt_ctrl_w
    | Escape, '\024' -> Done Alt_ctrl_x
    | Escape, '\025' -> Done Alt_ctrl_y
    | Escape, '\026' -> Done Alt_ctrl_z
    | Escape, '\027' -> Done Alt_escape
    | Escape, '\028' -> Done Alt_ctrl_antislash
    | Escape, '\029' -> Done Alt_ctrl_right_bracket
    | Escape, '\030' -> Done Alt_ctrl_caret
    | Escape, '\031' -> Done Alt_ctrl_underscore

    | Escape, ' ' -> Done Alt_space
    | Escape, '!' -> Done Alt_exclamation_mark
    | Escape, '"' -> Done Alt_double_quote
    | Escape, '#' -> Done Alt_hash
    | Escape, '$' -> Done Alt_dollar
    | Escape, '%' -> Done Alt_percent
    | Escape, '&' -> Done Alt_ampersand
    | Escape, '\'' -> Done Alt_quote
    | Escape, '(' -> Done Alt_left_parenthesis
    | Escape, ')' -> Done Alt_right_parenthesis
    | Escape, '*' -> Done Alt_star
    | Escape, '+' -> Done Alt_plus
    | Escape, ',' -> Done Alt_comma
    | Escape, '-' -> Done Alt_minus
    | Escape, '.' -> Done Alt_period
    | Escape, '/' -> Done Alt_slash
    | Escape, '0' -> Done Alt_digit_0
    | Escape, '1' -> Done Alt_digit_1
    | Escape, '2' -> Done Alt_digit_2
    | Escape, '3' -> Done Alt_digit_3
    | Escape, '4' -> Done Alt_digit_4
    | Escape, '5' -> Done Alt_digit_5
    | Escape, '6' -> Done Alt_digit_6
    | Escape, '7' -> Done Alt_digit_7
    | Escape, '8' -> Done Alt_digit_8
    | Escape, '9' -> Done Alt_digit_9
    | Escape, ':' -> Done Alt_colon
    | Escape, ';' -> Done Alt_semicolon
    | Escape, '<' -> Done Alt_left_chevron
    | Escape, '=' -> Done Alt_equal
    | Escape, '>' -> Done Alt_right_chevron
    | Escape, '?' -> Done Alt_question_mark
    | Escape, '@' -> Done Alt_arobase

    | Escape, 'A' -> Done Alt_letter_A
    | Escape, 'B' -> Done Alt_letter_B
    | Escape, 'C' -> Done Alt_letter_C
    | Escape, 'D' -> Done Alt_letter_D
    | Escape, 'E' -> Done Alt_letter_E
    | Escape, 'F' -> Done Alt_letter_F
    | Escape, 'G' -> Done Alt_letter_G
    | Escape, 'H' -> Done Alt_letter_H
    | Escape, 'I' -> Done Alt_letter_I
    | Escape, 'J' -> Done Alt_letter_J
    | Escape, 'K' -> Done Alt_letter_K
    | Escape, 'L' -> Done Alt_letter_L
    | Escape, 'M' -> Done Alt_letter_M
    | Escape, 'N' -> Done Alt_letter_N
    | Escape, 'O' -> Escape_O 0
    | Escape, 'P' -> Done Alt_letter_P
    | Escape, 'Q' -> Done Alt_letter_Q
    | Escape, 'R' -> Done Alt_letter_R
    | Escape, 'S' -> Done Alt_letter_S
    | Escape, 'T' -> Done Alt_letter_T
    | Escape, 'U' -> Done Alt_letter_U
    | Escape, 'V' -> Done Alt_letter_V
    | Escape, 'W' -> Done Alt_letter_W
    | Escape, 'X' -> Done Alt_letter_X
    | Escape, 'Y' -> Done Alt_letter_Y
    | Escape, 'Z' -> Done Alt_letter_Z

    | Escape, '[' -> Escape_bracket 0
    | Escape, '\\' -> Done Alt_antislash
    | Escape, ']' -> Done Alt_right_bracket
    | Escape, '^' -> Done Alt_caret
    | Escape, '_' -> Done Alt_underscore
    | Escape, '`' -> Done Alt_backquote

    | Escape, 'a' -> Done Alt_letter_a
    | Escape, 'b' -> Done Alt_letter_b
    | Escape, 'c' -> Done Alt_letter_c
    | Escape, 'd' -> Done Alt_letter_d
    | Escape, 'e' -> Done Alt_letter_e
    | Escape, 'f' -> Done Alt_letter_f
    | Escape, 'g' -> Done Alt_letter_g
    | Escape, 'h' -> Done Alt_letter_h
    | Escape, 'i' -> Done Alt_letter_i
    | Escape, 'j' -> Done Alt_letter_j
    | Escape, 'k' -> Done Alt_letter_k
    | Escape, 'l' -> Done Alt_letter_l
    | Escape, 'm' -> Done Alt_letter_m
    | Escape, 'n' -> Done Alt_letter_n
    | Escape, 'o' -> Done Alt_letter_o
    | Escape, 'p' -> Done Alt_letter_p
    | Escape, 'q' -> Done Alt_letter_q
    | Escape, 'r' -> Done Alt_letter_r
    | Escape, 's' -> Done Alt_letter_s
    | Escape, 't' -> Done Alt_letter_t
    | Escape, 'u' -> Done Alt_letter_u
    | Escape, 'v' -> Done Alt_letter_v
    | Escape, 'w' -> Done Alt_letter_w
    | Escape, 'x' -> Done Alt_letter_x
    | Escape, 'y' -> Done Alt_letter_y
    | Escape, 'z' -> Done Alt_letter_z

    | Escape, '{' -> Done Alt_left_brace
    | Escape, '|' -> Done Alt_pipe
    | Escape, '}' -> Done Alt_right_brace
    | Escape, '~' -> Done Alt_tilde
    | Escape, '\127' -> Done Alt_backspace

    (* Escape sequences with [. *)
    | Escape_bracket x, ('0' .. '9' as digit) ->
        Escape_bracket (x * 10 + Char.code digit - Char.code '0')
    | Escape_bracket x, ';' ->
        Escape_bracket_semi (x, 0)
    | Escape_bracket_semi (x, y), ('0' .. '9' as digit) ->
        Escape_bracket_semi (x, y * 10 + Char.code digit - Char.code '0')

    | Escape_bracket 0, 'A' -> Done Up
    | Escape_bracket_semi (1, 2), 'A' -> Done Shift_up
    | Escape_bracket_semi (1, 3), 'A' -> Done Alt_up
    | Escape_bracket_semi (1, 4), 'A' -> Done Alt_shift_up
    | Escape_bracket_semi (1, 5), 'A' -> Done Ctrl_up
    | Escape_bracket_semi (1, 6), 'A' -> Done Ctrl_shift_up
    | Escape_bracket_semi (1, 7), 'A' -> Done Ctrl_alt_up
    | Escape_bracket_semi (1, 8), 'A' -> Done Ctrl_alt_shift_up

    | Escape_bracket 0, 'B' -> Done Down
    | Escape_bracket_semi (1, 2), 'B' -> Done Shift_down
    | Escape_bracket_semi (1, 3), 'B' -> Done Alt_down
    | Escape_bracket_semi (1, 4), 'B' -> Done Alt_shift_down
    | Escape_bracket_semi (1, 5), 'B' -> Done Ctrl_down
    | Escape_bracket_semi (1, 6), 'B' -> Done Ctrl_shift_down
    | Escape_bracket_semi (1, 7), 'B' -> Done Ctrl_alt_down
    | Escape_bracket_semi (1, 8), 'B' -> Done Ctrl_alt_shift_down

    | Escape_bracket 0, 'C' -> Done Right
    | Escape_bracket_semi (1, 2), 'C' -> Done Shift_right
    | Escape_bracket_semi (1, 3), 'C' -> Done Alt_right
    | Escape_bracket_semi (1, 4), 'C' -> Done Alt_shift_right
    | Escape_bracket_semi (1, 5), 'C' -> Done Ctrl_right
    | Escape_bracket_semi (1, 6), 'C' -> Done Ctrl_shift_right
    | Escape_bracket_semi (1, 7), 'C' -> Done Ctrl_alt_right
    | Escape_bracket_semi (1, 8), 'C' -> Done Ctrl_alt_shift_right

    | Escape_bracket 0, 'D' -> Done Left
    | Escape_bracket_semi (1, 2), 'D' -> Done Shift_left
    | Escape_bracket_semi (1, 3), 'D' -> Done Alt_left
    | Escape_bracket_semi (1, 4), 'D' -> Done Alt_shift_left
    | Escape_bracket_semi (1, 5), 'D' -> Done Ctrl_left
    | Escape_bracket_semi (1, 6), 'D' -> Done Ctrl_shift_left
    | Escape_bracket_semi (1, 7), 'D' -> Done Ctrl_alt_left
    | Escape_bracket_semi (1, 8), 'D' -> Done Ctrl_alt_shift_left

    (* Sometimes [Escape_O 0, ...] instead, for some terminals. *)
    | Escape_bracket 0, 'F' -> Done End
    | Escape_bracket_semi (1, 2), 'F' -> Done Shift_end
    | Escape_bracket_semi (1, 3), 'F' -> Done Alt_end
    | Escape_bracket_semi (1, 4), 'F' -> Done Alt_shift_end
    | Escape_bracket_semi (1, 5), 'F' -> Done Ctrl_end
    | Escape_bracket_semi (1, 6), 'F' -> Done Ctrl_shift_end
    | Escape_bracket_semi (1, 7), 'F' -> Done Ctrl_alt_end
    | Escape_bracket_semi (1, 8), 'F' -> Done Ctrl_alt_shift_end

    (* Sometimes [Escape_O 0, ...] instead, for some terminals. *)
    | Escape_bracket 0, 'H' -> Done Home
    | Escape_bracket_semi (1, 2), 'H' -> Done Shift_home
    | Escape_bracket_semi (1, 3), 'H' -> Done Alt_home
    | Escape_bracket_semi (1, 4), 'H' -> Done Alt_shift_home
    | Escape_bracket_semi (1, 5), 'H' -> Done Ctrl_home
    | Escape_bracket_semi (1, 6), 'H' -> Done Ctrl_shift_home
    | Escape_bracket_semi (1, 7), 'H' -> Done Ctrl_alt_home
    | Escape_bracket_semi (1, 8), 'H' -> Done Ctrl_alt_shift_home

    | Escape_bracket 0, 'Z' -> Done Backtab

    | Escape_bracket 3, '~' -> Done Delete
    | Escape_bracket_semi (3, 2), '~' -> Done Shift_delete
    | Escape_bracket_semi (3, 3), '~' -> Done Alt_delete
    | Escape_bracket_semi (3, 4), '~' -> Done Alt_shift_delete
    | Escape_bracket_semi (3, 5), '~' -> Done Ctrl_delete
    | Escape_bracket_semi (3, 6), '~' -> Done Ctrl_shift_delete
    | Escape_bracket_semi (3, 7), '~' -> Done Ctrl_alt_delete
    | Escape_bracket_semi (3, 8), '~' -> Done Ctrl_alt_shift_delete

    | Escape_bracket 5, '~' -> Done Page_up
    | Escape_bracket_semi (5, 2), '~' -> Done Shift_page_up
    | Escape_bracket_semi (5, 3), '~' -> Done Alt_page_up
    | Escape_bracket_semi (5, 4), '~' -> Done Alt_shift_page_up
    | Escape_bracket_semi (5, 5), '~' -> Done Ctrl_page_up
    | Escape_bracket_semi (5, 6), '~' -> Done Ctrl_shift_page_up
    | Escape_bracket_semi (5, 7), '~' -> Done Ctrl_alt_page_up
    | Escape_bracket_semi (5, 8), '~' -> Done Ctrl_alt_shift_page_up

    | Escape_bracket 6, '~' -> Done Page_down
    | Escape_bracket_semi (6, 2), '~' -> Done Shift_page_down
    | Escape_bracket_semi (6, 3), '~' -> Done Alt_page_down
    | Escape_bracket_semi (6, 4), '~' -> Done Alt_shift_page_down
    | Escape_bracket_semi (6, 5), '~' -> Done Ctrl_page_down
    | Escape_bracket_semi (6, 6), '~' -> Done Ctrl_shift_page_down
    | Escape_bracket_semi (6, 7), '~' -> Done Ctrl_alt_page_down
    | Escape_bracket_semi (6, 8), '~' -> Done Ctrl_alt_shift_page_down

    | Escape_bracket 15, '~' -> Done F5
    | Escape_bracket_semi (15, 2), '~' -> Done Shift_f5
    | Escape_bracket_semi (15, 3), '~' -> Done Alt_f5
    | Escape_bracket_semi (15, 4), '~' -> Done Alt_shift_f5
    | Escape_bracket_semi (15, 5), '~' -> Done Ctrl_f5
    | Escape_bracket_semi (15, 6), '~' -> Done Ctrl_shift_f5
    | Escape_bracket_semi (15, 7), '~' -> Done Ctrl_alt_f5
    | Escape_bracket_semi (15, 8), '~' -> Done Ctrl_alt_shift_f5

    | Escape_bracket 17, '~' -> Done F6
    | Escape_bracket_semi (17, 2), '~' -> Done Shift_f6
    | Escape_bracket_semi (17, 3), '~' -> Done Alt_f6
    | Escape_bracket_semi (17, 4), '~' -> Done Alt_shift_f6
    | Escape_bracket_semi (17, 5), '~' -> Done Ctrl_f6
    | Escape_bracket_semi (17, 6), '~' -> Done Ctrl_shift_f6
    | Escape_bracket_semi (17, 7), '~' -> Done Ctrl_alt_f6
    | Escape_bracket_semi (17, 8), '~' -> Done Ctrl_alt_shift_f6

    | Escape_bracket 18, '~' -> Done F7
    | Escape_bracket_semi (18, 2), '~' -> Done Shift_f7
    | Escape_bracket_semi (18, 3), '~' -> Done Alt_f7
    | Escape_bracket_semi (18, 4), '~' -> Done Alt_shift_f7
    | Escape_bracket_semi (18, 5), '~' -> Done Ctrl_f7
    | Escape_bracket_semi (18, 6), '~' -> Done Ctrl_shift_f7
    | Escape_bracket_semi (18, 7), '~' -> Done Ctrl_alt_f7
    | Escape_bracket_semi (18, 8), '~' -> Done Ctrl_alt_shift_f7

    | Escape_bracket 19, '~' -> Done F8
    | Escape_bracket_semi (19, 2), '~' -> Done Shift_f8
    | Escape_bracket_semi (19, 3), '~' -> Done Alt_f8
    | Escape_bracket_semi (19, 4), '~' -> Done Alt_shift_f8
    | Escape_bracket_semi (19, 5), '~' -> Done Ctrl_f8
    | Escape_bracket_semi (19, 6), '~' -> Done Ctrl_shift_f8
    | Escape_bracket_semi (19, 7), '~' -> Done Ctrl_alt_f8
    | Escape_bracket_semi (19, 8), '~' -> Done Ctrl_alt_shift_f8

    | Escape_bracket 20, '~' -> Done F9
    | Escape_bracket_semi (20, 2), '~' -> Done Shift_f9
    | Escape_bracket_semi (20, 3), '~' -> Done Alt_f9
    | Escape_bracket_semi (20, 4), '~' -> Done Alt_shift_f9
    | Escape_bracket_semi (20, 5), '~' -> Done Ctrl_f9
    | Escape_bracket_semi (20, 6), '~' -> Done Ctrl_shift_f9
    | Escape_bracket_semi (20, 7), '~' -> Done Ctrl_alt_f9
    | Escape_bracket_semi (20, 8), '~' -> Done Ctrl_alt_shift_f9

    | Escape_bracket 21, '~' -> Done F10
    | Escape_bracket_semi (21, 2), '~' -> Done Shift_f10
    | Escape_bracket_semi (21, 3), '~' -> Done Alt_f10
    | Escape_bracket_semi (21, 4), '~' -> Done Alt_shift_f10
    | Escape_bracket_semi (21, 5), '~' -> Done Ctrl_f10
    | Escape_bracket_semi (21, 6), '~' -> Done Ctrl_shift_f10
    | Escape_bracket_semi (21, 7), '~' -> Done Ctrl_alt_f10
    | Escape_bracket_semi (21, 8), '~' -> Done Ctrl_alt_shift_f10

    | Escape_bracket 23, '~' -> Done F11
    | Escape_bracket_semi (23, 2), '~' -> Done Shift_f11
    | Escape_bracket_semi (23, 3), '~' -> Done Alt_f11
    | Escape_bracket_semi (23, 4), '~' -> Done Alt_shift_f11
    | Escape_bracket_semi (23, 5), '~' -> Done Ctrl_f11
    | Escape_bracket_semi (23, 6), '~' -> Done Ctrl_shift_f11
    | Escape_bracket_semi (23, 7), '~' -> Done Ctrl_alt_f11
    | Escape_bracket_semi (23, 8), '~' -> Done Ctrl_alt_shift_f11

    | Escape_bracket 24, '~' -> Done F12
    | Escape_bracket_semi (24, 2), '~' -> Done Shift_f12
    | Escape_bracket_semi (24, 3), '~' -> Done Alt_f12
    | Escape_bracket_semi (24, 4), '~' -> Done Alt_shift_f12
    | Escape_bracket_semi (24, 5), '~' -> Done Ctrl_f12
    | Escape_bracket_semi (24, 6), '~' -> Done Ctrl_shift_f12
    | Escape_bracket_semi (24, 7), '~' -> Done Ctrl_alt_f12
    | Escape_bracket_semi (24, 8), '~' -> Done Ctrl_alt_shift_f12

    | Escape_bracket_semi (y, x), 'R' -> Cursor_position (x, y)

    (* Escape sequences with O. *)
    | Escape_O x, ('0' .. '9' as digit) ->
        Escape_O (x * 10 + Char.code digit - Char.code '0')
    | Escape_O x, ';' ->
        Escape_O_semi (x, 0)
    | Escape_O_semi (x, y), ('0' .. '9' as digit) ->
        Escape_O_semi (x, y * 10 + Char.code digit - Char.code '0')

    | Escape_O 0, 'H' -> Done Home
    (* | Escape_O_semi (1, 2), 'H' -> Done Shift_home *)
    (* | Escape_O_semi (1, 3), 'H' -> Done Alt_home *)
    (* | Escape_O_semi (1, 4), 'H' -> Done Alt_shift_home *)
    (* | Escape_O_semi (1, 5), 'H' -> Done Ctrl_home *)
    (* | Escape_O_semi (1, 6), 'H' -> Done Ctrl_shift_home *)
    (* | Escape_O_semi (1, 7), 'H' -> Done Ctrl_alt_home *)
    (* | Escape_O_semi (1, 8), 'H' -> Done Ctrl_alt_shift_home *)

    | Escape_O 0, 'F' -> Done End
    (* | Escape_O_semi (1, 2), 'F' -> Done Shift_end *)
    (* | Escape_O_semi (1, 3), 'F' -> Done Alt_end *)
    (* | Escape_O_semi (1, 4), 'F' -> Done Alt_shift_end *)
    (* | Escape_O_semi (1, 5), 'F' -> Done Ctrl_end *)
    (* | Escape_O_semi (1, 6), 'F' -> Done Ctrl_shift_end *)
    (* | Escape_O_semi (1, 7), 'F' -> Done Ctrl_alt_end *)
    (* | Escape_O_semi (1, 8), 'F' -> Done Ctrl_alt_shift_end *)

    | Escape_O 0, 'P' -> Done F1
    | Escape_O_semi (1, 2), 'P' -> Done Shift_f1
    | Escape_O_semi (1, 3), 'P' -> Done Alt_f1
    | Escape_O_semi (1, 4), 'P' -> Done Alt_shift_f1
    | Escape_O_semi (1, 5), 'P' -> Done Ctrl_f1
    | Escape_O_semi (1, 6), 'P' -> Done Ctrl_shift_f1
    | Escape_O_semi (1, 7), 'P' -> Done Ctrl_alt_f1
    | Escape_O_semi (1, 8), 'P' -> Done Ctrl_alt_shift_f1

    | Escape_O 0, 'Q' -> Done F2
    | Escape_O_semi (1, 2), 'Q' -> Done Shift_f2
    | Escape_O_semi (1, 3), 'Q' -> Done Alt_f2
    | Escape_O_semi (1, 4), 'Q' -> Done Alt_shift_f2
    | Escape_O_semi (1, 5), 'Q' -> Done Ctrl_f2
    | Escape_O_semi (1, 6), 'Q' -> Done Ctrl_shift_f2
    | Escape_O_semi (1, 7), 'Q' -> Done Ctrl_alt_f2
    | Escape_O_semi (1, 8), 'Q' -> Done Ctrl_alt_shift_f2

    | Escape_O 0, 'R' -> Done F3
    | Escape_O_semi (1, 2), 'R' -> Done Shift_f3
    | Escape_O_semi (1, 3), 'R' -> Done Alt_f3
    | Escape_O_semi (1, 4), 'R' -> Done Alt_shift_f3
    | Escape_O_semi (1, 5), 'R' -> Done Ctrl_f3
    | Escape_O_semi (1, 6), 'R' -> Done Ctrl_shift_f3
    | Escape_O_semi (1, 7), 'R' -> Done Ctrl_alt_f3
    | Escape_O_semi (1, 8), 'R' -> Done Ctrl_alt_shift_f3

    | Escape_O 0, 'S' -> Done F4
    | Escape_O_semi (1, 2), 'S' -> Done Shift_f4
    | Escape_O_semi (1, 3), 'S' -> Done Alt_f4
    | Escape_O_semi (1, 4), 'S' -> Done Alt_shift_f4
    | Escape_O_semi (1, 5), 'S' -> Done Ctrl_f4
    | Escape_O_semi (1, 6), 'S' -> Done Ctrl_shift_f4
    | Escape_O_semi (1, 7), 'S' -> Done Ctrl_alt_f4
    | Escape_O_semi (1, 8), 'S' -> Done Ctrl_alt_shift_f4

    (* Unicode. *)
    | Empty, '\128' .. '\191' -> Invalid (state, char)
    | Empty, ('\192' .. '\223' as c) -> Unicode ([ c ], 1)
    | Empty, ('\224' .. '\239' as c) -> Unicode ([ c ], 2)
    | Empty, ('\240' .. '\247' as c) -> Unicode ([ c ], 3)
    | Empty, '\247' .. '\255' -> Invalid (state, char)

    | Escape, '\128' .. '\191' -> Invalid (state, char)
    | Escape, ('\192' .. '\223' as c) -> Escape_unicode ([ c ], 1)
    | Escape, ('\224' .. '\239' as c) -> Escape_unicode ([ c ], 2)
    | Escape, ('\240' .. '\247' as c) -> Escape_unicode ([ c ], 3)
    | Escape, '\247' .. '\255' -> Invalid (state, char)

    | Unicode (partial, _), '\000' .. '\127' -> Invalid (state, char)
    | Unicode (partial, remaining), '\128' .. '\191' ->
        if remaining = 1 then
          Done (Unicode (Misc.string_of_char_list_rev (char :: partial)))
        else
          Unicode (char :: partial, remaining - 1)
    | Unicode (partial, _), '\192' .. '\255' -> Invalid (state, char)

    | Escape_unicode (partial, _), '\000' .. '\127' -> Invalid (state, char)
    | Escape_unicode (partial, remaining), '\128' .. '\191' ->
        if remaining = 1 then
          Done (Alt_unicode (Misc.string_of_char_list_rev (char :: partial)))
        else
          Escape_unicode (char :: partial, remaining - 1)
    | Escape_unicode (partial, _), '\192' .. '\255' -> Invalid (state, char)

    (* Invalid or unsupported sequences. *)
    | Escape_bracket _, _
    | Escape_bracket_semi _, _
    | Escape_O _, _
    | Escape_O_semi _, _
    | Done _, _
    | Cursor_position _, _
    | Invalid _, _ -> Invalid (state, char)

(******************************************************************************)
(*                              Control Sequences                             *)
(******************************************************************************)

(* Cursor *)

let goto_bottom_right () =
  (* [goto_xy] could fail on some terminals as it is unspecified what happens
     when trying to move cursor outside the terminal. *)
  print_string "\027[999C\027[999B"

let query_cursor_position () =
  print_string "\027[6n"

let hide_cursor () =
  print_string "\027[?25l"

(* Clear *)

let clear_from_cursor () =
  print_string "\027[0J"

let clear_to_cursor () =
  print_string "\027[1J"

let clear_scrollback () =
  print_string "\027[3J"

(* Style *)

type intensity =
  | Normal
  | Bold
  | Faint

let intensity intensity =
  match intensity with
    | Normal -> print_string "\027[22m"
    | Bold -> print_string "\027[1m"
    | Faint -> print_string "\027[2m"

let underline value =
  if value then
    print_string "\027[4m"
  else
    print_string "\027[24m"

type color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let fg_color color =
  match color with
    | Default -> print_string "\027[39m"
    | Black -> print_string "\027[30m"
    | Red -> print_string "\027[31m"
    | Green -> print_string "\027[32m"
    | Yellow -> print_string "\027[33m"
    | Blue -> print_string "\027[34m"
    | Magenta -> print_string "\027[35m"
    | Cyan -> print_string "\027[36m"
    | White -> print_string "\027[37m"

let bg_color color =
  match color with
    | Default -> print_string "\027[49m"
    | Black -> print_string "\027[40m"
    | Red -> print_string "\027[41m"
    | Green -> print_string "\027[42m"
    | Yellow -> print_string "\027[43m"
    | Blue -> print_string "\027[44m"
    | Magenta -> print_string "\027[45m"
    | Cyan -> print_string "\027[46m"
    | White -> print_string "\027[47m"
