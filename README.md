# Red

A command-line text editor in OCaml.

## Help

Press Ctrl+H to display the help panel.
It lists key bindings for the current context, and you can follow links to get more information.

## Features

- Syntax highlighting for OCaml source files.
- Vertical and horizontal splits to show several files at the same time, or multiple parts of the same file.
- Multiple cursors, to edit several parts at the same time.
- Run external programs and display their output in the editor.
- UTF-8 encoding.
- Write configuration files in a simple language called RedL.

## Install

To compile Red you need to install the OCaml compiler.

On Debian or Ubuntu you can run:

    sudo apt-get install ocaml

Alternatively, you can use OPAM (OCaml Package Manager, https://opam.ocaml.org):

    sudo apt-get install opam
    opam init
    opam switch 4.03.0

Compile Red by running:

    make

A symbolic link is created to the executable. Run it with:

    ./red

You can specify filenames on the command-line to open them immediately.

## License

Red is released under the MIT license.
See the `LICENSE` file.
