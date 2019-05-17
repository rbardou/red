let echo s =
  let ch = open_out_gen [ Open_creat; Open_append; Open_wronly ] 0o640 "debug.log" in
  output_string ch s;
  output_char ch '\n';
  close_out ch

let echof x = Printf.ksprintf echo x
