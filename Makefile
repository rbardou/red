OCAMLBUILD := ocamlbuild
OCAMLBUILD := $(OCAMLBUILD) -no-links
OCAMLBUILD := $(OCAMLBUILD) -use-ocamlfind

default: example red

example:
	$(OCAMLBUILD) termlib/example.byte
	ln -sf _build/termlib/example.byte example

red:
	$(OCAMLBUILD) -I termlib src/main.byte
	ln -sf _build/src/main.byte red

clean:
	rm -rf _build example red

.PHONY: default example red clean
