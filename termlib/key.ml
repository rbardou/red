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
