all: camandelbrot

camandelbrot: main.ml
	ocamlfind ocamlopt -linkpkg -thread -package parmap -package graphics main.ml -o camandelbrot
