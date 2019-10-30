.PHONY: all clean


all:
	dune build main.bc.js


clean:
	rm -fv *.install
	rm -Rfv _build
	rm -fv .merlin
