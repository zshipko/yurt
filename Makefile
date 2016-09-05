flags=-I `ocamlfind query uri`,\
	  -I `ocamlfind query conduit`,\
	  -I `ocamlfind query lwt`,\
	  -I `ocamlfind query cohttp.lwt`,\
	  -I `ocamlfind query qe`

all:
	ocamlbuild -cflags "$(flags)" yurt.cmxa yurt.cma

install-qe:
	git clone https://github.com/zshipko/qe
	cd qe && $(MAKE) && $(MAKE) install

lib.byte:
	ocamlbuild -pkg qe -pkg cohttp.lwt yurt.cma

lib.native:
	ocamlbuild -cflags "$(flags)" yurt.cmxa

install:
	$(MAKE) uninstall || :
	cd _build && ocamlfind install yurt ../META yurt.cmxa yurt.cma *.cmx *.cmo *.cmi yurt.a
	rm -rf example/_build

uninstall:
	ocamlfind remove yurt

clean:
	rm -rf _build

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
