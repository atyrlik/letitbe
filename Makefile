default: build

build:
	ocamlbuild -use-ocamlfind main.byte

clean:
	ocamlbuild -clean
