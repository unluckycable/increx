.PHONY: all clean firefox


all:
	dune build main.bc.js

firefox:
	firefox ./index.html

clean:
	rm -fv *.install
	rm -Rfv _build
	rm -fv .merlin
