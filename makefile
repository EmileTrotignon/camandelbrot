all: camandelbrot

camandelbrot: main.ml histogram.ml color.ml
	eval $(opam config env); ocamlfind ocamlopt -linkpkg -thread -package parmap -package graphics color.ml histogram.ml main.ml -o camandelbrot
