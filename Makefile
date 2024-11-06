all:
	ocamlc -c src/expr.ml
	ocamlc -c src/types.ml
	ocamlc -o my_project src/expr.cmo src/types.cmo src/main.ml

clean:
	rm -f *.cmo *.cmi lambda_calculator
