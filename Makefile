flags=-I `ocamlfind query uri`,\
	  -I `ocamlfind query conduit`,\
	  -I `ocamlfind query lwt`,\
	  -I `ocamlfind query cohttp.lwt`,\
	  -I `ocamlfind query ezjsonm`

all:
	ocamlbuild -cflags "$(flags)" lib/yurt.cmxa lib/yurt.cma

install:
	$(MAKE) uninstall || :
	cd _build/lib && ocamlfind install yurt ../../META yurt.cmxa yurt.cma *.cmx *.cmo *.cmi yurt.a
	rm -rf example/_build

uninstall:
	ocamlfind remove yurt

clean:
	rm -rf _build

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
