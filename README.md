# Red

A command-line text editor in OCaml.

## Features

Basic features that you would expect from any editor:
- arrow keys, Home and End, Ctrl+arrow keys, Ctrl+Home and Ctrl+End (movement);
- Shift + movement key (selection);
- Return (insert new line / replace selection with new line);
- Delete and Backspace (delete selection or next or previous character);
- Ctrl+C, Ctrl+X, Ctrl+V (copy, cut and paste);
- Page Up and Page Down (scroll down and up);
- UTF-8 support;
- Ctrl+N to create a new file;
- Ctrl+O to open a file;
- Ctrl+S to save;
- Alt+Ctrl+S to save as;
- Ctrl+Z to undo, Ctrl+Y to redo;
- F2 to switch to another already opened file;
- Ctrl+K to delete end of line;
- Ctrl+D to delete one character (same as Delete);
- Alt+D to delete end of word;
- Alt+Backspace to delete beginning of word.

Press Alt+Escape to quit.

Advanced features:
- Alt + `"` to toggle multiple cursor mode (select several lines first to
  create one cursor per line);
- Alt + arrow keys: select another panel (for now, multiple panels are created
  if you give several files on the command-line);
- Alt+X to execute a command from its name (all commands are already bound though);
- Alt+Ctrl+X to execute an external program.

File selection (when opening or saving) has built-in auto-completion.
Use Up and Down to select one of the proposed filenames.
You may specify several filters by separating them with space characters.

## Install OCaml

To compile Red you need to install the OCaml compiler.

On Debian or Ubuntu you can run:

    sudo apt-get install ocaml

Alternatively, you can use OPAM (OCaml Package Manager, https://opam.ocaml.org):

    sudo apt-get install opam
    opam init
    opam switch 4.03.0

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
