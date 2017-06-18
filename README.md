# Red

A command-line text editor in OCaml.

## Features

This is a hobby project and it just started, so don't expect much.

Basic stuff that you would expect from any editor:
- arrow keys, Home and End (movement);
- Shift + movement key (selection);
- Return (insert new line / replace selection with new line);
- Delete and Backspace (delete selection or next or previous character);
- Ctrl+C, Ctrl+X, Ctrl+V (copy, cut and paste);
- Page Up and Page Down (scroll down and up);
- UTF-8 support;
- Ctrl+S to save (warning: no backup is created and there is no confirmation
  prompt yet).

Exotic stuff:
- Alt + arrow key to scroll by one unit (note: I use `xset r rate 250 60`
  to fasten my keyboard's repetition delay and rate, without it this is less
  useful);
- Alt + `"` to toggle multiple cursor mode (select several lines first to
  create one cursor per line);
- F5 to scroll to cursor (try pressing it several times).

## Install OCaml

To compile Red you need to install the OCaml compiler.

On Debian or Ubuntu you can run:

    sudo apt-get install ocaml

Alternatively, you can use OPAM (OCaml Package Manager, https://opam.ocaml.org):

    sudo apt-get install opam
    opam init
    opam switch 4.02.3

## Compile

Just run:

    make

## Run

A symbolic link is created to the executable. Run it with:

    ./red

Load a file with:

    ./red <filename>

## License

Red is released under the MIT license.
See the `LICENSE` file.
