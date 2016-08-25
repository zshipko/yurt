flags=-I `ocamlfind query uri`,\
	  -I `ocamlfind query conduit`,\
	  -I `ocamlfind query lwt`,\
	  -I `ocamlfind query cohttp.lwt`,\
	  -I `ocamlfind query qe`

all:  lib.byte lib.native

lib.byte:
	ocamlbuild -pkg qe -pkg cohttp.lwt hoof.cma

lib.native:
	ocamlbuild -cflags "$(flags)" hoof.cmxa

install:
	$(MAKE) uninstall || :
	cd _build && ocamlfind install hoof ../META hoof.cmxa hoof.cma *.cmx *.cmo *.cmi hoof.a

uninstall:
	ocamlfind remove hoof

clean:
	rm -rf _build

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
