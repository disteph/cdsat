all:
	ocamlc -c zipper.ml
	ocamlc -c formulae.ml
	ocamlc -c sequents.ml
	ocamlc -c search.ml
	ocamlc -c io.ml
	ocamlc -c test.ml
	ocamlc -o eurecaml.out zipper.cmo formulae.cmo sequents.cmo search.cmo io.cmo test.cmo
