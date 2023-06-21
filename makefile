all : ocaml

ocaml :
	ocaml app.ml

pdf : 
	- dot -Tpdf -o src/pdf/dotDec.pdf src/dot/dotDec.dot
	- dot -Tpdf -o src/pdf/dotBDD.pdf src/dot/dotBDD.dot

clean : 
	- rm -rf src/dot/*
	- rm -rf src/pdf/*