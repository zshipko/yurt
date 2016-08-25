flags=-I `ocamlfind query uri`,\
	  -I `ocamlfind query conduit`,\
	  -I `ocamlfind query lwt`,\
	  -I `ocamlfind query cohttp.lwt`,\
	  -I `ocamlfind query qe`

all:
	ocamlbuild -cflags "$(flags)" yurt.cmxa yurt.cma

lib.byte:
	ocamlbuild -pkg qe -pkg cohttp.lwt yurt.cma

lib.native:
	ocamlbuild -cflags "$(flags)" yurt.cmxa

install:
	$(MAKE) uninstall || :
	cd _build && ocamlfind install yurt ../META yurt.cmxa yurt.cma *.cmx *.cmo *.cmi yurt.a

uninstall:
	ocamlfind remove yurt

clean:
	rm -rf _build

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
