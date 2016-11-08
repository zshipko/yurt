flags=-I `ocamlfind query uri`,\
	  -I `ocamlfind query conduit`,\
	  -I `ocamlfind query lwt`,\
	  -I `ocamlfind query cohttp.lwt`,\
	  -I `ocamlfind query merz` \
	  -I `ocamlfind query irmin.unix` \
	  -I `ocamlfind query ezjsonm` \
	  -I `ocamlfind query ocamlgraph`

all:
	ocamlbuild -cflags "$(flags)" yurt.cmxa yurt.cma

install-merz:
	git clone https://github.com/twomblygroup/merz
	cd merz && $(MAKE) && $(MAKE) install

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
