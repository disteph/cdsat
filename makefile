all:
	ocamlc -c collection.ml
	ocamlc -c zipper.ml
	ocamlc -c formulae.ml
	ocamlc -c sequents.ml
	ocamlc -c strategy.ml
	ocamlc -c search.ml
	ocamlc -c io.ml
	ocamlc -c test.ml
	ocamlc -c main.ml
	ocamlc -o main.out zipper.cmo formulae.cmo collection.ml sequents.cmo strategy.cmo search.cmo io.cmo test.ml main.ml
