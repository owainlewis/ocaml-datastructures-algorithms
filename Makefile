clean:
	ocamlbuild -clean
	rm -rf bin

build:
	ocamlbuild  -use-ocamlfind src/ods.native
