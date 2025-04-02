OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

all: main

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

parser.cmi: parser.mli
	$(OCAMLC) -c parser.mli

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c parser.ml

lexer.cmo: lexer.ml
	$(OCAMLC) -c lexer.ml

typechecker.cmo: typechecker.ml
	$(OCAMLC) -c typechecker.ml

interpreter.cmo: interpreter.ml
	$(OCAMLC) -c interpreter.ml

main.cmo: main.ml
	$(OCAMLC) -c main.ml

main: ast.cmo parser.cmo lexer.cmo typechecker.cmo interpreter.cmo main.cmo
	$(OCAMLC) -o main ast.cmo parser.cmo lexer.cmo typechecker.cmo interpreter.cmo main.cmo

run: main
	./main

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml main
