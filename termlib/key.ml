type t =
  | Ctrl_arobase
  | Ctrl_a
  | Ctrl_b
  | Ctrl_c
  | Ctrl_d
  | Ctrl_e
  | Ctrl_f
  | Ctrl_g
  | Ctrl_h
  | Tab
  | Ctrl_j
  | Ctrl_k
  | Ctrl_l
  | Return
  | Ctrl_n
  | Ctrl_o
  | Ctrl_p
  | Ctrl_q
  | Ctrl_r
  | Ctrl_s
  | Ctrl_t
  | Ctrl_u
  | Ctrl_v
  | Ctrl_w
  | Ctrl_x
  | Ctrl_y
  | Ctrl_z
  | Escape
  | Ctrl_antislash
  | Ctrl_right_bracket
  | Ctrl_caret
  | Ctrl_underscore

  | Space
  | Exclamation_mark
  | Double_quote
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | Quote
  | Left_parenthesis
  | Right_parenthesis
  | Star
  | Plus
  | Comma
  | Minus
  | Period
  | Slash
  | Digit_0
  | Digit_1
  | Digit_2
  | Digit_3
  | Digit_4
  | Digit_5
  | Digit_6
  | Digit_7
  | Digit_8
  | Digit_9
  | Colon
  | Semicolon
  | Left_chevron
  | Equal
  | Right_chevron
  | Question_mark
  | Arobase

  | Letter_A
  | Letter_B
  | Letter_C
  | Letter_D
  | Letter_E
  | Letter_F
  | Letter_G
  | Letter_H
  | Letter_I
  | Letter_J
  | Letter_K
  | Letter_L
  | Letter_M
  | Letter_N
  | Letter_O
  | Letter_P
  | Letter_Q
  | Letter_R
  | Letter_S
  | Letter_T
  | Letter_U
  | Letter_V
  | Letter_W
  | Letter_X
  | Letter_Y
  | Letter_Z

  | Left_bracket
  | Antislash
  | Right_bracket
  | Caret
  | Underscore
  | Backquote

  | Letter_a
  | Letter_b
  | Letter_c
  | Letter_d
  | Letter_e
  | Letter_f
  | Letter_g
  | Letter_h
  | Letter_i
  | Letter_j
  | Letter_k
  | Letter_l
  | Letter_m
  | Letter_n
  | Letter_o
  | Letter_p
  | Letter_q
  | Letter_r
  | Letter_s
  | Letter_t
  | Letter_u
  | Letter_v
  | Letter_w
  | Letter_x
  | Letter_y
  | Letter_z

  | Left_brace
  | Pipe
  | Right_brace
  | Tilde
  | Backspace

  | Alt_ctrl_arobase
  | Alt_ctrl_a
  | Alt_ctrl_b
  | Alt_ctrl_c
  | Alt_ctrl_d
  | Alt_ctrl_e
  | Alt_ctrl_f
  | Alt_ctrl_g
  | Alt_ctrl_h
  | Alt_tab
  | Alt_ctrl_j
  | Alt_ctrl_k
  | Alt_ctrl_l
  | Alt_ctrl_m
  | Alt_ctrl_n
  | Alt_ctrl_o
  | Alt_ctrl_p
  | Alt_ctrl_q
  | Alt_ctrl_r
  | Alt_ctrl_s
  | Alt_ctrl_t
  | Alt_ctrl_u
  | Alt_ctrl_v
  | Alt_ctrl_w
  | Alt_ctrl_x
  | Alt_ctrl_y
  | Alt_ctrl_z
  | Alt_escape
  | Alt_ctrl_antislash
  | Alt_ctrl_right_bracket
  | Alt_ctrl_caret
  | Alt_ctrl_underscore

  | Alt_space
  | Alt_exclamation_mark
  | Alt_double_quote
  | Alt_hash
  | Alt_dollar
  | Alt_percent
  | Alt_ampersand
  | Alt_quote
  | Alt_left_parenthesis
  | Alt_right_parenthesis
  | Alt_star
  | Alt_plus
  | Alt_comma
  | Alt_minus
  | Alt_period
  | Alt_slash
  | Alt_digit_0
  | Alt_digit_1
  | Alt_digit_2
  | Alt_digit_3
  | Alt_digit_4
  | Alt_digit_5
  | Alt_digit_6
  | Alt_digit_7
  | Alt_digit_8
  | Alt_digit_9
  | Alt_colon
  | Alt_semicolon
  | Alt_left_chevron
  | Alt_equal
  | Alt_right_chevron
  | Alt_question_mark
  | Alt_arobase

  | Alt_letter_A
  | Alt_letter_B
  | Alt_letter_C
  | Alt_letter_D
  | Alt_letter_E
  | Alt_letter_F
  | Alt_letter_G
  | Alt_letter_H
  | Alt_letter_I
  | Alt_letter_J
  | Alt_letter_K
  | Alt_letter_L
  | Alt_letter_M
  | Alt_letter_N
  | Alt_letter_O
  | Alt_letter_P
  | Alt_letter_Q
  | Alt_letter_R
  | Alt_letter_S
  | Alt_letter_T
  | Alt_letter_U
  | Alt_letter_V
  | Alt_letter_W
  | Alt_letter_X
  | Alt_letter_Y
  | Alt_letter_Z

  | Alt_left_bracket
  | Alt_antislash
  | Alt_right_bracket
  | Alt_caret
  | Alt_underscore
  | Alt_backquote

  | Alt_letter_a
  | Alt_letter_b
  | Alt_letter_c
  | Alt_letter_d
  | Alt_letter_e
  | Alt_letter_f
  | Alt_letter_g
  | Alt_letter_h
  | Alt_letter_i
  | Alt_letter_j
  | Alt_letter_k
  | Alt_letter_l
  | Alt_letter_m
  | Alt_letter_n
  | Alt_letter_o
  | Alt_letter_p
  | Alt_letter_q
  | Alt_letter_r
  | Alt_letter_s
  | Alt_letter_t
  | Alt_letter_u
  | Alt_letter_v
  | Alt_letter_w
  | Alt_letter_x
  | Alt_letter_y
  | Alt_letter_z

  | Alt_left_brace
  | Alt_pipe
  | Alt_right_brace
  | Alt_tilde
  | Alt_backspace

  | Up
  | Shift_up
  | Alt_up
  | Alt_shift_up
  | Ctrl_up
  | Ctrl_shift_up
  | Ctrl_alt_up
  | Ctrl_alt_shift_up

  | Down
  | Shift_down
  | Alt_down
  | Alt_shift_down
  | Ctrl_down
  | Ctrl_shift_down
  | Ctrl_alt_down
  | Ctrl_alt_shift_down

  | Right
  | Shift_right
  | Alt_right
  | Alt_shift_right
  | Ctrl_right
  | Ctrl_shift_right
  | Ctrl_alt_right
  | Ctrl_alt_shift_right

  | Left
  | Shift_left
  | Alt_left
  | Alt_shift_left
  | Ctrl_left
  | Ctrl_shift_left
  | Ctrl_alt_left
  | Ctrl_alt_shift_left

  | Delete
  | Shift_delete
  | Alt_delete
  | Alt_shift_delete
  | Ctrl_delete
  | Ctrl_shift_delete
  | Ctrl_alt_delete
  | Ctrl_alt_shift_delete

  | Page_up
  | Shift_page_up
  | Alt_page_up
  | Alt_shift_page_up
  | Ctrl_page_up
  | Ctrl_shift_page_up
  | Ctrl_alt_page_up
  | Ctrl_alt_shift_page_up

  | Page_down
  | Shift_page_down
  | Alt_page_down
  | Alt_shift_page_down
  | Ctrl_page_down
  | Ctrl_shift_page_down
  | Ctrl_alt_page_down
  | Ctrl_alt_shift_page_down

  | Home
  | Shift_home
  | Alt_home
  | Alt_shift_home
  | Ctrl_home
  | Ctrl_shift_home
  | Ctrl_alt_home
  | Ctrl_alt_shift_home

  | End
  | Shift_end
  | Alt_end
  | Alt_shift_end
  | Ctrl_end
  | Ctrl_shift_end
  | Ctrl_alt_end
  | Ctrl_alt_shift_end

  | F1
  | Shift_f1
  | Alt_f1
  | Alt_shift_f1
  | Ctrl_f1
  | Ctrl_shift_f1
  | Ctrl_alt_f1
  | Ctrl_alt_shift_f1

  | F2
  | Shift_f2
  | Alt_f2
  | Alt_shift_f2
  | Ctrl_f2
  | Ctrl_shift_f2
  | Ctrl_alt_f2
  | Ctrl_alt_shift_f2

  | F3
  | Shift_f3
  | Alt_f3
  | Alt_shift_f3
  | Ctrl_f3
  | Ctrl_shift_f3
  | Ctrl_alt_f3
  | Ctrl_alt_shift_f3

  | F4
  | Shift_f4
  | Alt_f4
  | Alt_shift_f4
  | Ctrl_f4
  | Ctrl_shift_f4
  | Ctrl_alt_f4
  | Ctrl_alt_shift_f4

  | F5
  | Shift_f5
  | Alt_f5
  | Alt_shift_f5
  | Ctrl_f5
  | Ctrl_shift_f5
  | Ctrl_alt_f5
  | Ctrl_alt_shift_f5

  | F6
  | Shift_f6
  | Alt_f6
  | Alt_shift_f6
  | Ctrl_f6
  | Ctrl_shift_f6
  | Ctrl_alt_f6
  | Ctrl_alt_shift_f6

  | F7
  | Shift_f7
  | Alt_f7
  | Alt_shift_f7
  | Ctrl_f7
  | Ctrl_shift_f7
  | Ctrl_alt_f7
  | Ctrl_alt_shift_f7

  | F8
  | Shift_f8
  | Alt_f8
  | Alt_shift_f8
  | Ctrl_f8
  | Ctrl_shift_f8
  | Ctrl_alt_f8
  | Ctrl_alt_shift_f8

  | F9
  | Shift_f9
  | Alt_f9
  | Alt_shift_f9
  | Ctrl_f9
  | Ctrl_shift_f9
  | Ctrl_alt_f9
  | Ctrl_alt_shift_f9

  | F10
  | Shift_f10
  | Alt_f10
  | Alt_shift_f10
  | Ctrl_f10
  | Ctrl_shift_f10
  | Ctrl_alt_f10
  | Ctrl_alt_shift_f10

  | F11
  | Shift_f11
  | Alt_f11
  | Alt_shift_f11
  | Ctrl_f11
  | Ctrl_shift_f11
  | Ctrl_alt_f11
  | Ctrl_alt_shift_f11

  | F12
  | Shift_f12
  | Alt_f12
  | Alt_shift_f12
  | Ctrl_f12
  | Ctrl_shift_f12
  | Ctrl_alt_f12
  | Ctrl_alt_shift_f12

  | Backtab

  | Unicode of string
  | Alt_unicode of string

let show key =
  match key with
    | Ctrl_arobase -> "Ctrl_arobase"
    | Ctrl_a -> "Ctrl_a"
    | Ctrl_b -> "Ctrl_b"
    | Ctrl_c -> "Ctrl_c"
    | Ctrl_d -> "Ctrl_d"
    | Ctrl_e -> "Ctrl_e"
    | Ctrl_f -> "Ctrl_f"
    | Ctrl_g -> "Ctrl_g"
    | Ctrl_h -> "Ctrl_h"
    | Tab -> "Tab"
    | Ctrl_j -> "Ctrl_j"
    | Ctrl_k -> "Ctrl_k"
    | Ctrl_l -> "Ctrl_l"
    | Return -> "Return"
    | Ctrl_n -> "Ctrl_n"
    | Ctrl_o -> "Ctrl_o"
    | Ctrl_p -> "Ctrl_p"
    | Ctrl_q -> "Ctrl_q"
    | Ctrl_r -> "Ctrl_r"
    | Ctrl_s -> "Ctrl_s"
    | Ctrl_t -> "Ctrl_t"
    | Ctrl_u -> "Ctrl_u"
    | Ctrl_v -> "Ctrl_v"
    | Ctrl_w -> "Ctrl_w"
    | Ctrl_x -> "Ctrl_x"
    | Ctrl_y -> "Ctrl_y"
    | Ctrl_z -> "Ctrl_z"
    | Escape -> "Escape"
    | Ctrl_antislash -> "Ctrl_antislash"
    | Ctrl_right_bracket -> "Ctrl_right_bracket"
    | Ctrl_caret -> "Ctrl_caret"
    | Ctrl_underscore -> "Ctrl_underscore"

    | Space -> "Space"
    | Exclamation_mark -> "Exclamation_mark"
    | Double_quote -> "Double_quote"
    | Hash -> "Hash"
    | Dollar -> "Dollar"
    | Percent -> "Percent"
    | Ampersand -> "Ampersand"
    | Quote -> "Quote"
    | Left_parenthesis -> "Left_parenthesis"
    | Right_parenthesis -> "Right_parenthesis"
    | Star -> "Star"
    | Plus -> "Plus"
    | Comma -> "Comma"
    | Minus -> "Minus"
    | Period -> "Period"
    | Slash -> "Slash"
    | Digit_0 -> "Digit_0"
    | Digit_1 -> "Digit_1"
    | Digit_2 -> "Digit_2"
    | Digit_3 -> "Digit_3"
    | Digit_4 -> "Digit_4"
    | Digit_5 -> "Digit_5"
    | Digit_6 -> "Digit_6"
    | Digit_7 -> "Digit_7"
    | Digit_8 -> "Digit_8"
    | Digit_9 -> "Digit_9"
    | Colon -> "Colon"
    | Semicolon -> "Semicolon"
    | Left_chevron -> "Left_chevron"
    | Equal -> "Equal"
    | Right_chevron -> "Right_chevron"
    | Question_mark -> "Question_mark"
    | Arobase -> "Arobase"

    | Letter_A -> "Letter_A"
    | Letter_B -> "Letter_B"
    | Letter_C -> "Letter_C"
    | Letter_D -> "Letter_D"
    | Letter_E -> "Letter_E"
    | Letter_F -> "Letter_F"
    | Letter_G -> "Letter_G"
    | Letter_H -> "Letter_H"
    | Letter_I -> "Letter_I"
    | Letter_J -> "Letter_J"
    | Letter_K -> "Letter_K"
    | Letter_L -> "Letter_L"
    | Letter_M -> "Letter_M"
    | Letter_N -> "Letter_N"
    | Letter_O -> "Letter_O"
    | Letter_P -> "Letter_P"
    | Letter_Q -> "Letter_Q"
    | Letter_R -> "Letter_R"
    | Letter_S -> "Letter_S"
    | Letter_T -> "Letter_T"
    | Letter_U -> "Letter_U"
    | Letter_V -> "Letter_V"
    | Letter_W -> "Letter_W"
    | Letter_X -> "Letter_X"
    | Letter_Y -> "Letter_Y"
    | Letter_Z -> "Letter_Z"

    | Left_bracket -> "Left_bracket"
    | Antislash -> "Antislash"
    | Right_bracket -> "Right_bracket"
    | Caret -> "Caret"
    | Underscore -> "Underscore"
    | Backquote -> "Backquote"

    | Letter_a -> "Letter_a"
    | Letter_b -> "Letter_b"
    | Letter_c -> "Letter_c"
    | Letter_d -> "Letter_d"
    | Letter_e -> "Letter_e"
    | Letter_f -> "Letter_f"
    | Letter_g -> "Letter_g"
    | Letter_h -> "Letter_h"
    | Letter_i -> "Letter_i"
    | Letter_j -> "Letter_j"
    | Letter_k -> "Letter_k"
    | Letter_l -> "Letter_l"
    | Letter_m -> "Letter_m"
    | Letter_n -> "Letter_n"
    | Letter_o -> "Letter_o"
    | Letter_p -> "Letter_p"
    | Letter_q -> "Letter_q"
    | Letter_r -> "Letter_r"
    | Letter_s -> "Letter_s"
    | Letter_t -> "Letter_t"
    | Letter_u -> "Letter_u"
    | Letter_v -> "Letter_v"
    | Letter_w -> "Letter_w"
    | Letter_x -> "Letter_x"
    | Letter_y -> "Letter_y"
    | Letter_z -> "Letter_z"

    | Left_brace -> "Left_brace"
    | Pipe -> "Pipe"
    | Right_brace -> "Right_brace"
    | Tilde -> "Tilde"
    | Backspace -> "Backspace"

    | Alt_ctrl_arobase -> "Alt_ctrl_arobase"
    | Alt_ctrl_a -> "Alt_ctrl_a"
    | Alt_ctrl_b -> "Alt_ctrl_b"
    | Alt_ctrl_c -> "Alt_ctrl_c"
    | Alt_ctrl_d -> "Alt_ctrl_d"
    | Alt_ctrl_e -> "Alt_ctrl_e"
    | Alt_ctrl_f -> "Alt_ctrl_f"
    | Alt_ctrl_g -> "Alt_ctrl_g"
    | Alt_ctrl_h -> "Alt_ctrl_h"
    | Alt_tab -> "Alt_tab"
    | Alt_ctrl_j -> "Alt_ctrl_j"
    | Alt_ctrl_k -> "Alt_ctrl_k"
    | Alt_ctrl_l -> "Alt_ctrl_l"
    | Alt_ctrl_m -> "Alt_ctrl_m"
    | Alt_ctrl_n -> "Alt_ctrl_n"
    | Alt_ctrl_o -> "Alt_ctrl_o"
    | Alt_ctrl_p -> "Alt_ctrl_p"
    | Alt_ctrl_q -> "Alt_ctrl_q"
    | Alt_ctrl_r -> "Alt_ctrl_r"
    | Alt_ctrl_s -> "Alt_ctrl_s"
    | Alt_ctrl_t -> "Alt_ctrl_t"
    | Alt_ctrl_u -> "Alt_ctrl_u"
    | Alt_ctrl_v -> "Alt_ctrl_v"
    | Alt_ctrl_w -> "Alt_ctrl_w"
    | Alt_ctrl_x -> "Alt_ctrl_x"
    | Alt_ctrl_y -> "Alt_ctrl_y"
    | Alt_ctrl_z -> "Alt_ctrl_z"
    | Alt_escape -> "Alt_escape"
    | Alt_ctrl_antislash -> "Alt_ctrl_antislash"
    | Alt_ctrl_right_bracket -> "Alt_ctrl_right_bracket"
    | Alt_ctrl_caret -> "Alt_ctrl_caret"
    | Alt_ctrl_underscore -> "Alt_ctrl_underscore"

    | Alt_space -> "Alt_space"
    | Alt_exclamation_mark -> "Alt_exclamation_mark"
    | Alt_double_quote -> "Alt_double_quote"
    | Alt_hash -> "Alt_hash"
    | Alt_dollar -> "Alt_dollar"
    | Alt_percent -> "Alt_percent"
    | Alt_ampersand -> "Alt_ampersand"
    | Alt_quote -> "Alt_quote"
    | Alt_left_parenthesis -> "Alt_left_parenthesis"
    | Alt_right_parenthesis -> "Alt_right_parenthesis"
    | Alt_star -> "Alt_star"
    | Alt_plus -> "Alt_plus"
    | Alt_comma -> "Alt_comma"
    | Alt_minus -> "Alt_minus"
    | Alt_period -> "Alt_period"
    | Alt_slash -> "Alt_slash"
    | Alt_digit_0 -> "Alt_digit_0"
    | Alt_digit_1 -> "Alt_digit_1"
    | Alt_digit_2 -> "Alt_digit_2"
    | Alt_digit_3 -> "Alt_digit_3"
    | Alt_digit_4 -> "Alt_digit_4"
    | Alt_digit_5 -> "Alt_digit_5"
    | Alt_digit_6 -> "Alt_digit_6"
    | Alt_digit_7 -> "Alt_digit_7"
    | Alt_digit_8 -> "Alt_digit_8"
    | Alt_digit_9 -> "Alt_digit_9"
    | Alt_colon -> "Alt_colon"
    | Alt_semicolon -> "Alt_semicolon"
    | Alt_left_chevron -> "Alt_left_chevron"
    | Alt_equal -> "Alt_equal"
    | Alt_right_chevron -> "Alt_right_chevron"
    | Alt_question_mark -> "Alt_question_mark"
    | Alt_arobase -> "Alt_arobase"

    | Alt_letter_A -> "Alt_letter_A"
    | Alt_letter_B -> "Alt_letter_B"
    | Alt_letter_C -> "Alt_letter_C"
    | Alt_letter_D -> "Alt_letter_D"
    | Alt_letter_E -> "Alt_letter_E"
    | Alt_letter_F -> "Alt_letter_F"
    | Alt_letter_G -> "Alt_letter_G"
    | Alt_letter_H -> "Alt_letter_H"
    | Alt_letter_I -> "Alt_letter_I"
    | Alt_letter_J -> "Alt_letter_J"
    | Alt_letter_K -> "Alt_letter_K"
    | Alt_letter_L -> "Alt_letter_L"
    | Alt_letter_M -> "Alt_letter_M"
    | Alt_letter_N -> "Alt_letter_N"
    | Alt_letter_O -> "Alt_letter_O"
    | Alt_letter_P -> "Alt_letter_P"
    | Alt_letter_Q -> "Alt_letter_Q"
    | Alt_letter_R -> "Alt_letter_R"
    | Alt_letter_S -> "Alt_letter_S"
    | Alt_letter_T -> "Alt_letter_T"
    | Alt_letter_U -> "Alt_letter_U"
    | Alt_letter_V -> "Alt_letter_V"
    | Alt_letter_W -> "Alt_letter_W"
    | Alt_letter_X -> "Alt_letter_X"
    | Alt_letter_Y -> "Alt_letter_Y"
    | Alt_letter_Z -> "Alt_letter_Z"

    | Alt_left_bracket -> "Alt_left_bracket"
    | Alt_antislash -> "Alt_antislash"
    | Alt_right_bracket -> "Alt_right_bracket"
    | Alt_caret -> "Alt_caret"
    | Alt_underscore -> "Alt_underscore"
    | Alt_backquote -> "Alt_backquote"

    | Alt_letter_a -> "Alt_letter_a"
    | Alt_letter_b -> "Alt_letter_b"
    | Alt_letter_c -> "Alt_letter_c"
    | Alt_letter_d -> "Alt_letter_d"
    | Alt_letter_e -> "Alt_letter_e"
    | Alt_letter_f -> "Alt_letter_f"
    | Alt_letter_g -> "Alt_letter_g"
    | Alt_letter_h -> "Alt_letter_h"
    | Alt_letter_i -> "Alt_letter_i"
    | Alt_letter_j -> "Alt_letter_j"
    | Alt_letter_k -> "Alt_letter_k"
    | Alt_letter_l -> "Alt_letter_l"
    | Alt_letter_m -> "Alt_letter_m"
    | Alt_letter_n -> "Alt_letter_n"
    | Alt_letter_o -> "Alt_letter_o"
    | Alt_letter_p -> "Alt_letter_p"
    | Alt_letter_q -> "Alt_letter_q"
    | Alt_letter_r -> "Alt_letter_r"
    | Alt_letter_s -> "Alt_letter_s"
    | Alt_letter_t -> "Alt_letter_t"
    | Alt_letter_u -> "Alt_letter_u"
    | Alt_letter_v -> "Alt_letter_v"
    | Alt_letter_w -> "Alt_letter_w"
    | Alt_letter_x -> "Alt_letter_x"
    | Alt_letter_y -> "Alt_letter_y"
    | Alt_letter_z -> "Alt_letter_z"

    | Alt_left_brace -> "Alt_left_brace"
    | Alt_pipe -> "Alt_pipe"
    | Alt_right_brace -> "Alt_right_brace"
    | Alt_tilde -> "Alt_tilde"
    | Alt_backspace -> "Alt_backspace"

    | Up -> "Up"
    | Shift_up -> "Shift_up"
    | Alt_up -> "Alt_up"
    | Alt_shift_up -> "Alt_shift_up"
    | Ctrl_up -> "Ctrl_up"
    | Ctrl_shift_up -> "Ctrl_shift_up"
    | Ctrl_alt_up -> "Ctrl_alt_up"
    | Ctrl_alt_shift_up -> "Ctrl_alt_shift_up"

    | Down -> "Down"
    | Shift_down -> "Shift_down"
    | Alt_down -> "Alt_down"
    | Alt_shift_down -> "Alt_shift_down"
    | Ctrl_down -> "Ctrl_down"
    | Ctrl_shift_down -> "Ctrl_shift_down"
    | Ctrl_alt_down -> "Ctrl_alt_down"
    | Ctrl_alt_shift_down -> "Ctrl_alt_shift_down"

    | Right -> "Right"
    | Shift_right -> "Shift_right"
    | Alt_right -> "Alt_right"
    | Alt_shift_right -> "Alt_shift_right"
    | Ctrl_right -> "Ctrl_right"
    | Ctrl_shift_right -> "Ctrl_shift_right"
    | Ctrl_alt_right -> "Ctrl_alt_right"
    | Ctrl_alt_shift_right -> "Ctrl_alt_shift_right"

    | Left -> "Left"
    | Shift_left -> "Shift_left"
    | Alt_left -> "Alt_left"
    | Alt_shift_left -> "Alt_shift_left"
    | Ctrl_left -> "Ctrl_left"
    | Ctrl_shift_left -> "Ctrl_shift_left"
    | Ctrl_alt_left -> "Ctrl_alt_left"
    | Ctrl_alt_shift_left -> "Ctrl_alt_shift_left"

    | Delete -> "Delete"
    | Shift_delete -> "Shift_delete"
    | Alt_delete -> "Alt_delete"
    | Alt_shift_delete -> "Alt_shift_delete"
    | Ctrl_delete -> "Ctrl_delete"
    | Ctrl_shift_delete -> "Ctrl_shift_delete"
    | Ctrl_alt_delete -> "Ctrl_alt_delete"
    | Ctrl_alt_shift_delete -> "Ctrl_alt_shift_delete"

    | Page_up -> "Page_up"
    | Shift_page_up -> "Shift_page_up"
    | Alt_page_up -> "Alt_page_up"
    | Alt_shift_page_up -> "Alt_shift_page_up"
    | Ctrl_page_up -> "Ctrl_page_up"
    | Ctrl_shift_page_up -> "Ctrl_shift_page_up"
    | Ctrl_alt_page_up -> "Ctrl_alt_page_up"
    | Ctrl_alt_shift_page_up -> "Ctrl_alt_shift_page_up"

    | Page_down -> "Page_down"
    | Shift_page_down -> "Shift_page_down"
    | Alt_page_down -> "Alt_page_down"
    | Alt_shift_page_down -> "Alt_shift_page_down"
    | Ctrl_page_down -> "Ctrl_page_down"
    | Ctrl_shift_page_down -> "Ctrl_shift_page_down"
    | Ctrl_alt_page_down -> "Ctrl_alt_page_down"
    | Ctrl_alt_shift_page_down -> "Ctrl_alt_shift_page_down"

    | Home -> "Home"
    | Shift_home -> "Shift_home"
    | Alt_home -> "Alt_home"
    | Alt_shift_home -> "Alt_shift_home"
    | Ctrl_home -> "Ctrl_home"
    | Ctrl_shift_home -> "Ctrl_shift_home"
    | Ctrl_alt_home -> "Ctrl_alt_home"
    | Ctrl_alt_shift_home -> "Ctrl_alt_shift_home"

    | End -> "End"
    | Shift_end -> "Shift_end"
    | Alt_end -> "Alt_end"
    | Alt_shift_end -> "Alt_shift_end"
    | Ctrl_end -> "Ctrl_end"
    | Ctrl_shift_end -> "Ctrl_shift_end"
    | Ctrl_alt_end -> "Ctrl_alt_end"
    | Ctrl_alt_shift_end -> "Ctrl_alt_shift_end"

    | F1 -> "F1"
    | Shift_f1 -> "Shift_f1"
    | Alt_f1 -> "Alt_f1"
    | Alt_shift_f1 -> "Alt_shift_f1"
    | Ctrl_f1 -> "Ctrl_f1"
    | Ctrl_shift_f1 -> "Ctrl_shift_f1"
    | Ctrl_alt_f1 -> "Ctrl_alt_f1"
    | Ctrl_alt_shift_f1 -> "Ctrl_alt_shift_f1"

    | F2 -> "F2"
    | Shift_f2 -> "Shift_f2"
    | Alt_f2 -> "Alt_f2"
    | Alt_shift_f2 -> "Alt_shift_f2"
    | Ctrl_f2 -> "Ctrl_f2"
    | Ctrl_shift_f2 -> "Ctrl_shift_f2"
    | Ctrl_alt_f2 -> "Ctrl_alt_f2"
    | Ctrl_alt_shift_f2 -> "Ctrl_alt_shift_f2"

    | F3 -> "F3"
    | Shift_f3 -> "Shift_f3"
    | Alt_f3 -> "Alt_f3"
    | Alt_shift_f3 -> "Alt_shift_f3"
    | Ctrl_f3 -> "Ctrl_f3"
    | Ctrl_shift_f3 -> "Ctrl_shift_f3"
    | Ctrl_alt_f3 -> "Ctrl_alt_f3"
    | Ctrl_alt_shift_f3 -> "Ctrl_alt_shift_f3"

    | F4 -> "F4"
    | Shift_f4 -> "Shift_f4"
    | Alt_f4 -> "Alt_f4"
    | Alt_shift_f4 -> "Alt_shift_f4"
    | Ctrl_f4 -> "Ctrl_f4"
    | Ctrl_shift_f4 -> "Ctrl_shift_f4"
    | Ctrl_alt_f4 -> "Ctrl_alt_f4"
    | Ctrl_alt_shift_f4 -> "Ctrl_alt_shift_f4"

    | F5 -> "F5"
    | Shift_f5 -> "Shift_f5"
    | Alt_f5 -> "Alt_f5"
    | Alt_shift_f5 -> "Alt_shift_f5"
    | Ctrl_f5 -> "Ctrl_f5"
    | Ctrl_shift_f5 -> "Ctrl_shift_f5"
    | Ctrl_alt_f5 -> "Ctrl_alt_f5"
    | Ctrl_alt_shift_f5 -> "Ctrl_alt_shift_f5"

    | F6 -> "F6"
    | Shift_f6 -> "Shift_f6"
    | Alt_f6 -> "Alt_f6"
    | Alt_shift_f6 -> "Alt_shift_f6"
    | Ctrl_f6 -> "Ctrl_f6"
    | Ctrl_shift_f6 -> "Ctrl_shift_f6"
    | Ctrl_alt_f6 -> "Ctrl_alt_f6"
    | Ctrl_alt_shift_f6 -> "Ctrl_alt_shift_f6"

    | F7 -> "F7"
    | Shift_f7 -> "Shift_f7"
    | Alt_f7 -> "Alt_f7"
    | Alt_shift_f7 -> "Alt_shift_f7"
    | Ctrl_f7 -> "Ctrl_f7"
    | Ctrl_shift_f7 -> "Ctrl_shift_f7"
    | Ctrl_alt_f7 -> "Ctrl_alt_f7"
    | Ctrl_alt_shift_f7 -> "Ctrl_alt_shift_f7"

    | F8 -> "F8"
    | Shift_f8 -> "Shift_f8"
    | Alt_f8 -> "Alt_f8"
    | Alt_shift_f8 -> "Alt_shift_f8"
    | Ctrl_f8 -> "Ctrl_f8"
    | Ctrl_shift_f8 -> "Ctrl_shift_f8"
    | Ctrl_alt_f8 -> "Ctrl_alt_f8"
    | Ctrl_alt_shift_f8 -> "Ctrl_alt_shift_f8"

    | F9 -> "F9"
    | Shift_f9 -> "Shift_f9"
    | Alt_f9 -> "Alt_f9"
    | Alt_shift_f9 -> "Alt_shift_f9"
    | Ctrl_f9 -> "Ctrl_f9"
    | Ctrl_shift_f9 -> "Ctrl_shift_f9"
    | Ctrl_alt_f9 -> "Ctrl_alt_f9"
    | Ctrl_alt_shift_f9 -> "Ctrl_alt_shift_f9"

    | F10 -> "F10"
    | Shift_f10 -> "Shift_f10"
    | Alt_f10 -> "Alt_f10"
    | Alt_shift_f10 -> "Alt_shift_f10"
    | Ctrl_f10 -> "Ctrl_f10"
    | Ctrl_shift_f10 -> "Ctrl_shift_f10"
    | Ctrl_alt_f10 -> "Ctrl_alt_f10"
    | Ctrl_alt_shift_f10 -> "Ctrl_alt_shift_f10"

    | F11 -> "F11"
    | Shift_f11 -> "Shift_f11"
    | Alt_f11 -> "Alt_f11"
    | Alt_shift_f11 -> "Alt_shift_f11"
    | Ctrl_f11 -> "Ctrl_f11"
    | Ctrl_shift_f11 -> "Ctrl_shift_f11"
    | Ctrl_alt_f11 -> "Ctrl_alt_f11"
    | Ctrl_alt_shift_f11 -> "Ctrl_alt_shift_f11"

    | F12 -> "F12"
    | Shift_f12 -> "Shift_f12"
    | Alt_f12 -> "Alt_f12"
    | Alt_shift_f12 -> "Alt_shift_f12"
    | Ctrl_f12 -> "Ctrl_f12"
    | Ctrl_shift_f12 -> "Ctrl_shift_f12"
    | Ctrl_alt_f12 -> "Ctrl_alt_f12"
    | Ctrl_alt_shift_f12 -> "Ctrl_alt_shift_f12"

    | Backtab -> "Backtab"

    | Unicode s -> Printf.sprintf "Unicode %S" s
    | Alt_unicode s -> Printf.sprintf "Alt_unicode %S" s

type decomposition =
  {
    name: string;
    ctrl: bool;
    alt: bool;
    shift: bool;
  }

let decompose key =
  let name name = { name; ctrl = false; alt = false; shift = false } in
  let ctrl name = { name; ctrl = true; alt = false; shift = false } in
  let alt name = { name; ctrl = false; alt = true; shift = false } in
  let shift name = { name; ctrl = false; alt = false; shift = true } in
  let ctrl_shift name = { name; ctrl = true; alt = false; shift = true } in
  let ctrl_alt name = { name; ctrl = true; alt = true; shift = false } in
  let alt_ctrl name = { name; ctrl = true; alt = true; shift = false } in
  let alt_shift name = { name; ctrl = false; alt = true; shift = true } in
  let ctrl_alt_shift name = { name; ctrl = true; alt = true; shift = true } in
  match key with
    | Ctrl_arobase -> ctrl "@"
    | Ctrl_a -> ctrl "A"
    | Ctrl_b -> ctrl "B"
    | Ctrl_c -> ctrl "C"
    | Ctrl_d -> ctrl "D"
    | Ctrl_e -> ctrl "E"
    | Ctrl_f -> ctrl "F"
    | Ctrl_g -> ctrl "G"
    | Ctrl_h -> ctrl "H"
    | Tab -> name "Tab"
    | Ctrl_j -> ctrl "J"
    | Ctrl_k -> ctrl "K"
    | Ctrl_l -> ctrl "L"
    | Return -> name "Return"
    | Ctrl_n -> ctrl "N"
    | Ctrl_o -> ctrl "O"
    | Ctrl_p -> ctrl "P"
    | Ctrl_q -> ctrl "Q"
    | Ctrl_r -> ctrl "R"
    | Ctrl_s -> ctrl "S"
    | Ctrl_t -> ctrl "T"
    | Ctrl_u -> ctrl "U"
    | Ctrl_v -> ctrl "V"
    | Ctrl_w -> ctrl "W"
    | Ctrl_x -> ctrl "X"
    | Ctrl_y -> ctrl "Y"
    | Ctrl_z -> ctrl "Z"
    | Escape -> name "Escape"
    | Ctrl_antislash -> ctrl "\\"
    | Ctrl_right_bracket -> ctrl "]"
    | Ctrl_caret -> ctrl "^"
    | Ctrl_underscore -> ctrl "_"

    | Space -> name "Space"
    | Exclamation_mark -> name "!"
    | Double_quote -> name "\""
    | Hash -> name "#"
    | Dollar -> name "$"
    | Percent -> name "%"
    | Ampersand -> name "&"
    | Quote -> name "'"
    | Left_parenthesis -> name "("
    | Right_parenthesis -> name ")"
    | Star -> name "*"
    | Plus -> name "+"
    | Comma -> name ","
    | Minus -> name "-"
    | Period -> name "."
    | Slash -> name "/"
    | Digit_0 -> name "0"
    | Digit_1 -> name "1"
    | Digit_2 -> name "2"
    | Digit_3 -> name "3"
    | Digit_4 -> name "4"
    | Digit_5 -> name "5"
    | Digit_6 -> name "6"
    | Digit_7 -> name "7"
    | Digit_8 -> name "8"
    | Digit_9 -> name "9"
    | Colon -> name ":"
    | Semicolon -> name ";"
    | Left_chevron -> name "<"
    | Equal -> name "="
    | Right_chevron -> name ">"
    | Question_mark -> name "?"
    | Arobase -> name "@"

    | Letter_A -> shift "A"
    | Letter_B -> shift "B"
    | Letter_C -> shift "C"
    | Letter_D -> shift "D"
    | Letter_E -> shift "E"
    | Letter_F -> shift "F"
    | Letter_G -> shift "G"
    | Letter_H -> shift "H"
    | Letter_I -> shift "I"
    | Letter_J -> shift "J"
    | Letter_K -> shift "K"
    | Letter_L -> shift "L"
    | Letter_M -> shift "M"
    | Letter_N -> shift "N"
    | Letter_O -> shift "O"
    | Letter_P -> shift "P"
    | Letter_Q -> shift "Q"
    | Letter_R -> shift "R"
    | Letter_S -> shift "S"
    | Letter_T -> shift "T"
    | Letter_U -> shift "U"
    | Letter_V -> shift "V"
    | Letter_W -> shift "W"
    | Letter_X -> shift "X"
    | Letter_Y -> shift "Y"
    | Letter_Z -> shift "Z"

    | Left_bracket -> name "["
    | Antislash -> name "\\"
    | Right_bracket -> name "]"
    | Caret -> name "^"
    | Underscore -> name "_"
    | Backquote -> name "`"

    | Letter_a -> name "A"
    | Letter_b -> name "B"
    | Letter_c -> name "C"
    | Letter_d -> name "D"
    | Letter_e -> name "E"
    | Letter_f -> name "F"
    | Letter_g -> name "G"
    | Letter_h -> name "H"
    | Letter_i -> name "I"
    | Letter_j -> name "J"
    | Letter_k -> name "K"
    | Letter_l -> name "L"
    | Letter_m -> name "M"
    | Letter_n -> name "N"
    | Letter_o -> name "O"
    | Letter_p -> name "P"
    | Letter_q -> name "Q"
    | Letter_r -> name "R"
    | Letter_s -> name "S"
    | Letter_t -> name "T"
    | Letter_u -> name "U"
    | Letter_v -> name "V"
    | Letter_w -> name "W"
    | Letter_x -> name "X"
    | Letter_y -> name "Y"
    | Letter_z -> name "Z"

    | Left_brace -> name "{"
    | Pipe -> name "|"
    | Right_brace -> name "}"
    | Tilde -> name "~"
    | Backspace -> name "Backspace"

    | Alt_ctrl_arobase -> alt_ctrl "@"
    | Alt_ctrl_a -> alt_ctrl "A"
    | Alt_ctrl_b -> alt_ctrl "B"
    | Alt_ctrl_c -> alt_ctrl "C"
    | Alt_ctrl_d -> alt_ctrl "D"
    | Alt_ctrl_e -> alt_ctrl "E"
    | Alt_ctrl_f -> alt_ctrl "F"
    | Alt_ctrl_g -> alt_ctrl "G"
    | Alt_ctrl_h -> alt_ctrl "H"
    | Alt_tab -> alt "Tab"
    | Alt_ctrl_j -> alt_ctrl "J"
    | Alt_ctrl_k -> alt_ctrl "K"
    | Alt_ctrl_l -> alt_ctrl "L"
    | Alt_ctrl_m -> alt_ctrl "M"
    | Alt_ctrl_n -> alt_ctrl "N"
    | Alt_ctrl_o -> alt_ctrl "O"
    | Alt_ctrl_p -> alt_ctrl "P"
    | Alt_ctrl_q -> alt_ctrl "Q"
    | Alt_ctrl_r -> alt_ctrl "R"
    | Alt_ctrl_s -> alt_ctrl "S"
    | Alt_ctrl_t -> alt_ctrl "T"
    | Alt_ctrl_u -> alt_ctrl "U"
    | Alt_ctrl_v -> alt_ctrl "V"
    | Alt_ctrl_w -> alt_ctrl "W"
    | Alt_ctrl_x -> alt_ctrl "X"
    | Alt_ctrl_y -> alt_ctrl "Y"
    | Alt_ctrl_z -> alt_ctrl "Z"
    | Alt_escape -> alt "Escape"
    | Alt_ctrl_antislash -> alt_ctrl "\\"
    | Alt_ctrl_right_bracket -> alt_ctrl "]"
    | Alt_ctrl_caret -> alt_ctrl "^"
    | Alt_ctrl_underscore -> alt_ctrl "_"

    | Alt_space -> alt "Space"
    | Alt_exclamation_mark -> alt "!"
    | Alt_double_quote -> alt "\""
    | Alt_hash -> alt "#"
    | Alt_dollar -> alt "$"
    | Alt_percent -> alt "%"
    | Alt_ampersand -> alt "&"
    | Alt_quote -> alt "'"
    | Alt_left_parenthesis -> alt "("
    | Alt_right_parenthesis -> alt ")"
    | Alt_star -> alt "*"
    | Alt_plus -> alt "+"
    | Alt_comma -> alt ","
    | Alt_minus -> alt "-"
    | Alt_period -> alt "."
    | Alt_slash -> alt "/"
    | Alt_digit_0 -> alt "0"
    | Alt_digit_1 -> alt "1"
    | Alt_digit_2 -> alt "2"
    | Alt_digit_3 -> alt "3"
    | Alt_digit_4 -> alt "4"
    | Alt_digit_5 -> alt "5"
    | Alt_digit_6 -> alt "6"
    | Alt_digit_7 -> alt "7"
    | Alt_digit_8 -> alt "8"
    | Alt_digit_9 -> alt "9"
    | Alt_colon -> alt ":"
    | Alt_semicolon -> alt ";"
    | Alt_left_chevron -> alt "<"
    | Alt_equal -> alt "="
    | Alt_right_chevron -> alt ">"
    | Alt_question_mark -> alt "?"
    | Alt_arobase -> alt "@"

    | Alt_letter_A -> alt_shift "A"
    | Alt_letter_B -> alt_shift "B"
    | Alt_letter_C -> alt_shift "C"
    | Alt_letter_D -> alt_shift "D"
    | Alt_letter_E -> alt_shift "E"
    | Alt_letter_F -> alt_shift "F"
    | Alt_letter_G -> alt_shift "G"
    | Alt_letter_H -> alt_shift "H"
    | Alt_letter_I -> alt_shift "I"
    | Alt_letter_J -> alt_shift "J"
    | Alt_letter_K -> alt_shift "K"
    | Alt_letter_L -> alt_shift "L"
    | Alt_letter_M -> alt_shift "M"
    | Alt_letter_N -> alt_shift "N"
    | Alt_letter_O -> alt_shift "O"
    | Alt_letter_P -> alt_shift "P"
    | Alt_letter_Q -> alt_shift "Q"
    | Alt_letter_R -> alt_shift "R"
    | Alt_letter_S -> alt_shift "S"
    | Alt_letter_T -> alt_shift "T"
    | Alt_letter_U -> alt_shift "U"
    | Alt_letter_V -> alt_shift "V"
    | Alt_letter_W -> alt_shift "W"
    | Alt_letter_X -> alt_shift "X"
    | Alt_letter_Y -> alt_shift "Y"
    | Alt_letter_Z -> alt_shift "Z"

    | Alt_left_bracket -> alt "["
    | Alt_antislash -> alt "\\"
    | Alt_right_bracket -> alt "]"
    | Alt_caret -> alt "^"
    | Alt_underscore -> alt "_"
    | Alt_backquote -> alt "`"

    | Alt_letter_a -> alt "A"
    | Alt_letter_b -> alt "B"
    | Alt_letter_c -> alt "C"
    | Alt_letter_d -> alt "D"
    | Alt_letter_e -> alt "E"
    | Alt_letter_f -> alt "F"
    | Alt_letter_g -> alt "G"
    | Alt_letter_h -> alt "H"
    | Alt_letter_i -> alt "I"
    | Alt_letter_j -> alt "J"
    | Alt_letter_k -> alt "K"
    | Alt_letter_l -> alt "L"
    | Alt_letter_m -> alt "M"
    | Alt_letter_n -> alt "N"
    | Alt_letter_o -> alt "O"
    | Alt_letter_p -> alt "P"
    | Alt_letter_q -> alt "Q"
    | Alt_letter_r -> alt "R"
    | Alt_letter_s -> alt "S"
    | Alt_letter_t -> alt "T"
    | Alt_letter_u -> alt "U"
    | Alt_letter_v -> alt "V"
    | Alt_letter_w -> alt "W"
    | Alt_letter_x -> alt "X"
    | Alt_letter_y -> alt "Y"
    | Alt_letter_z -> alt "Z"

    | Alt_left_brace -> alt "{"
    | Alt_pipe -> alt "|"
    | Alt_right_brace -> alt "}"
    | Alt_tilde -> alt "~"
    | Alt_backspace -> alt "Backspace"

    | Up -> name "Up"
    | Shift_up -> shift "Up"
    | Alt_up -> alt "Up"
    | Alt_shift_up -> alt_shift "Up"
    | Ctrl_up -> ctrl "Up"
    | Ctrl_shift_up -> ctrl_shift "Up"
    | Ctrl_alt_up -> ctrl_alt "Up"
    | Ctrl_alt_shift_up -> ctrl_alt_shift "Up"

    | Down -> name "Down"
    | Shift_down -> shift "Down"
    | Alt_down -> alt "Down"
    | Alt_shift_down -> alt_shift "Down"
    | Ctrl_down -> ctrl "Down"
    | Ctrl_shift_down -> ctrl_shift "Down"
    | Ctrl_alt_down -> ctrl_alt "Down"
    | Ctrl_alt_shift_down -> ctrl_alt_shift "Down"

    | Right -> name "Right"
    | Shift_right -> shift "Right"
    | Alt_right -> alt "Right"
    | Alt_shift_right -> alt_shift "Right"
    | Ctrl_right -> ctrl "Right"
    | Ctrl_shift_right -> ctrl_shift "Right"
    | Ctrl_alt_right -> ctrl_alt "Right"
    | Ctrl_alt_shift_right -> ctrl_alt_shift "Right"

    | Left -> name "Left"
    | Shift_left -> shift "Left"
    | Alt_left -> alt "Left"
    | Alt_shift_left -> alt_shift "Left"
    | Ctrl_left -> ctrl "Left"
    | Ctrl_shift_left -> ctrl_shift "Left"
    | Ctrl_alt_left -> ctrl_alt "Left"
    | Ctrl_alt_shift_left -> ctrl_alt_shift "Left"

    | Delete -> name "Delete"
    | Shift_delete -> shift "Delete"
    | Alt_delete -> alt "Delete"
    | Alt_shift_delete -> alt_shift "Delete"
    | Ctrl_delete -> ctrl "Delete"
    | Ctrl_shift_delete -> ctrl_shift "Delete"
    | Ctrl_alt_delete -> ctrl_alt "Delete"
    | Ctrl_alt_shift_delete -> ctrl_alt_shift "Delete"

    | Page_up -> name "PageUp"
    | Shift_page_up -> shift "PageUp"
    | Alt_page_up -> alt "PageUp"
    | Alt_shift_page_up -> alt_shift "PageUp"
    | Ctrl_page_up -> ctrl "PageUp"
    | Ctrl_shift_page_up -> ctrl_shift "PageUp"
    | Ctrl_alt_page_up -> ctrl_alt "PageUp"
    | Ctrl_alt_shift_page_up -> ctrl_alt_shift "PageUp"

    | Page_down -> name "PageDown"
    | Shift_page_down -> shift "PageDown"
    | Alt_page_down -> alt "PageDown"
    | Alt_shift_page_down -> alt_shift "PageDown"
    | Ctrl_page_down -> ctrl "PageDown"
    | Ctrl_shift_page_down -> ctrl_shift "PageDown"
    | Ctrl_alt_page_down -> ctrl_alt "PageDown"
    | Ctrl_alt_shift_page_down -> ctrl_alt_shift "PageDown"

    | Home -> name "Home"
    | Shift_home -> shift "Home"
    | Alt_home -> alt "Home"
    | Alt_shift_home -> alt_shift "Home"
    | Ctrl_home -> ctrl "Home"
    | Ctrl_shift_home -> ctrl_shift "Home"
    | Ctrl_alt_home -> ctrl_alt "Home"
    | Ctrl_alt_shift_home -> ctrl_alt_shift "Home"

    | End -> name "End"
    | Shift_end -> shift "End"
    | Alt_end -> alt "End"
    | Alt_shift_end -> alt_shift "End"
    | Ctrl_end -> ctrl "End"
    | Ctrl_shift_end -> ctrl_shift "End"
    | Ctrl_alt_end -> ctrl_alt "End"
    | Ctrl_alt_shift_end -> ctrl_alt_shift "End"

    | F1 -> name "F1"
    | Shift_f1 -> shift "F1"
    | Alt_f1 -> alt "F1"
    | Alt_shift_f1 -> alt_shift "F1"
    | Ctrl_f1 -> ctrl "F1"
    | Ctrl_shift_f1 -> ctrl_shift "F1"
    | Ctrl_alt_f1 -> ctrl_alt "F1"
    | Ctrl_alt_shift_f1 -> ctrl_alt_shift "F1"

    | F2 -> name "F2"
    | Shift_f2 -> shift "F2"
    | Alt_f2 -> alt "F2"
    | Alt_shift_f2 -> alt_shift "F2"
    | Ctrl_f2 -> ctrl "F2"
    | Ctrl_shift_f2 -> ctrl_shift "F2"
    | Ctrl_alt_f2 -> ctrl_alt "F2"
    | Ctrl_alt_shift_f2 -> ctrl_alt_shift "F2"

    | F3 -> name "F3"
    | Shift_f3 -> shift "F3"
    | Alt_f3 -> alt "F3"
    | Alt_shift_f3 -> alt_shift "F3"
    | Ctrl_f3 -> ctrl "F3"
    | Ctrl_shift_f3 -> ctrl_shift "F3"
    | Ctrl_alt_f3 -> ctrl_alt "F3"
    | Ctrl_alt_shift_f3 -> ctrl_alt_shift "F3"

    | F4 -> name "F4"
    | Shift_f4 -> shift "F4"
    | Alt_f4 -> alt "F4"
    | Alt_shift_f4 -> alt_shift "F4"
    | Ctrl_f4 -> ctrl "F4"
    | Ctrl_shift_f4 -> ctrl_shift "F4"
    | Ctrl_alt_f4 -> ctrl_alt "F4"
    | Ctrl_alt_shift_f4 -> ctrl_alt_shift "F4"

    | F5 -> name "F5"
    | Shift_f5 -> shift "F5"
    | Alt_f5 -> alt "F5"
    | Alt_shift_f5 -> alt_shift "F5"
    | Ctrl_f5 -> ctrl "F5"
    | Ctrl_shift_f5 -> ctrl_shift "F5"
    | Ctrl_alt_f5 -> ctrl_alt "F5"
    | Ctrl_alt_shift_f5 -> ctrl_alt_shift "F5"

    | F6 -> name "F6"
    | Shift_f6 -> shift "F6"
    | Alt_f6 -> alt "F6"
    | Alt_shift_f6 -> alt_shift "F6"
    | Ctrl_f6 -> ctrl "F6"
    | Ctrl_shift_f6 -> ctrl_shift "F6"
    | Ctrl_alt_f6 -> ctrl_alt "F6"
    | Ctrl_alt_shift_f6 -> ctrl_alt_shift "F6"

    | F7 -> name "F7"
    | Shift_f7 -> shift "F7"
    | Alt_f7 -> alt "F7"
    | Alt_shift_f7 -> alt_shift "F7"
    | Ctrl_f7 -> ctrl "F7"
    | Ctrl_shift_f7 -> ctrl_shift "F7"
    | Ctrl_alt_f7 -> ctrl_alt "F7"
    | Ctrl_alt_shift_f7 -> ctrl_alt_shift "F7"

    | F8 -> name "F8"
    | Shift_f8 -> shift "F8"
    | Alt_f8 -> alt "F8"
    | Alt_shift_f8 -> alt_shift "F8"
    | Ctrl_f8 -> ctrl "F8"
    | Ctrl_shift_f8 -> ctrl_shift "F8"
    | Ctrl_alt_f8 -> ctrl_alt "F8"
    | Ctrl_alt_shift_f8 -> ctrl_alt_shift "F8"

    | F9 -> name "F9"
    | Shift_f9 -> shift "F9"
    | Alt_f9 -> alt "F9"
    | Alt_shift_f9 -> alt_shift "F9"
    | Ctrl_f9 -> ctrl "F9"
    | Ctrl_shift_f9 -> ctrl_shift "F9"
    | Ctrl_alt_f9 -> ctrl_alt "F9"
    | Ctrl_alt_shift_f9 -> ctrl_alt_shift "F9"

    | F10 -> name "F10"
    | Shift_f10 -> shift "F10"
    | Alt_f10 -> alt "F10"
    | Alt_shift_f10 -> alt_shift "F10"
    | Ctrl_f10 -> ctrl "F10"
    | Ctrl_shift_f10 -> ctrl_shift "F10"
    | Ctrl_alt_f10 -> ctrl_alt "F10"
    | Ctrl_alt_shift_f10 -> ctrl_alt_shift "F10"

    | F11 -> name "F11"
    | Shift_f11 -> shift "F11"
    | Alt_f11 -> alt "F11"
    | Alt_shift_f11 -> alt_shift "F11"
    | Ctrl_f11 -> ctrl "F11"
    | Ctrl_shift_f11 -> ctrl_shift "F11"
    | Ctrl_alt_f11 -> ctrl_alt "F11"
    | Ctrl_alt_shift_f11 -> ctrl_alt_shift "F11"

    | F12 -> name "F12"
    | Shift_f12 -> shift "F12"
    | Alt_f12 -> alt "F12"
    | Alt_shift_f12 -> alt_shift "F12"
    | Ctrl_f12 -> ctrl "F12"
    | Ctrl_shift_f12 -> ctrl_shift "F12"
    | Ctrl_alt_f12 -> ctrl_alt "F12"
    | Ctrl_alt_shift_f12 -> ctrl_alt_shift "F12"

    | Backtab -> name "Backtab"

    | Unicode s -> name s
    | Alt_unicode s -> alt s

let display key =
  let key = decompose key in
  let name = key.name in
  let name = if key.shift then "Shift+" ^ name else name in
  let name = if key.ctrl then "Ctrl+" ^ name else name in
  let name = if key.alt then "Alt+" ^ name else name in
  name

type symbol =
  | ASCII of char
  | Unicode of string
  | Control

let show_symbol symbol =
  match symbol with
    | ASCII char -> "ASCII " ^ String.make 1 char
    | Unicode s -> Printf.sprintf "Unicode %S" s
    | Control -> "Control"

let symbol key =
  match key with
    | Space -> ASCII ' '
    | Exclamation_mark -> ASCII '!'
    | Double_quote -> ASCII '"'
    | Hash -> ASCII '#'
    | Dollar -> ASCII '$'
    | Percent -> ASCII '%'
    | Ampersand -> ASCII '&'
    | Quote -> ASCII '\''
    | Left_parenthesis -> ASCII '('
    | Right_parenthesis -> ASCII ')'
    | Star -> ASCII '*'
    | Plus -> ASCII '+'
    | Comma -> ASCII ','
    | Minus -> ASCII '-'
    | Period -> ASCII '.'
    | Slash -> ASCII '/'
    | Digit_0 -> ASCII '0'
    | Digit_1 -> ASCII '1'
    | Digit_2 -> ASCII '2'
    | Digit_3 -> ASCII '3'
    | Digit_4 -> ASCII '4'
    | Digit_5 -> ASCII '5'
    | Digit_6 -> ASCII '6'
    | Digit_7 -> ASCII '7'
    | Digit_8 -> ASCII '8'
    | Digit_9 -> ASCII '9'
    | Colon -> ASCII ':'
    | Semicolon -> ASCII ';'
    | Left_chevron -> ASCII '<'
    | Equal -> ASCII '='
    | Right_chevron -> ASCII '>'
    | Question_mark -> ASCII '?'
    | Arobase -> ASCII '@'

    | Letter_A -> ASCII 'A'
    | Letter_B -> ASCII 'B'
    | Letter_C -> ASCII 'C'
    | Letter_D -> ASCII 'D'
    | Letter_E -> ASCII 'E'
    | Letter_F -> ASCII 'F'
    | Letter_G -> ASCII 'G'
    | Letter_H -> ASCII 'H'
    | Letter_I -> ASCII 'I'
    | Letter_J -> ASCII 'J'
    | Letter_K -> ASCII 'K'
    | Letter_L -> ASCII 'L'
    | Letter_M -> ASCII 'M'
    | Letter_N -> ASCII 'N'
    | Letter_O -> ASCII 'O'
    | Letter_P -> ASCII 'P'
    | Letter_Q -> ASCII 'Q'
    | Letter_R -> ASCII 'R'
    | Letter_S -> ASCII 'S'
    | Letter_T -> ASCII 'T'
    | Letter_U -> ASCII 'U'
    | Letter_V -> ASCII 'V'
    | Letter_W -> ASCII 'W'
    | Letter_X -> ASCII 'X'
    | Letter_Y -> ASCII 'Y'
    | Letter_Z -> ASCII 'Z'

    | Left_bracket -> ASCII '['
    | Antislash -> ASCII '\\'
    | Right_bracket -> ASCII ']'
    | Caret -> ASCII '^'
    | Underscore -> ASCII '_'
    | Backquote -> ASCII '`'

    | Letter_a -> ASCII 'a'
    | Letter_b -> ASCII 'b'
    | Letter_c -> ASCII 'c'
    | Letter_d -> ASCII 'd'
    | Letter_e -> ASCII 'e'
    | Letter_f -> ASCII 'f'
    | Letter_g -> ASCII 'g'
    | Letter_h -> ASCII 'h'
    | Letter_i -> ASCII 'i'
    | Letter_j -> ASCII 'j'
    | Letter_k -> ASCII 'k'
    | Letter_l -> ASCII 'l'
    | Letter_m -> ASCII 'm'
    | Letter_n -> ASCII 'n'
    | Letter_o -> ASCII 'o'
    | Letter_p -> ASCII 'p'
    | Letter_q -> ASCII 'q'
    | Letter_r -> ASCII 'r'
    | Letter_s -> ASCII 's'
    | Letter_t -> ASCII 't'
    | Letter_u -> ASCII 'u'
    | Letter_v -> ASCII 'v'
    | Letter_w -> ASCII 'w'
    | Letter_x -> ASCII 'x'
    | Letter_y -> ASCII 'y'
    | Letter_z -> ASCII 'z'

    | Left_brace -> ASCII '['
    | Pipe -> ASCII '|'
    | Right_brace -> ASCII ']'
    | Tilde -> ASCII '~'

    | Unicode s -> Unicode s

    | _ -> Control

module Ordered =
struct
  type key = t
  type t = key
  let compare = (Pervasives.compare: t -> t -> int)
end

module Set = Set.Make (Ordered)

module Map =
struct
  include Map.Make (Ordered)

  let of_list bindings =
    List.fold_left (fun acc (key, value) -> add key value acc) empty bindings
end
