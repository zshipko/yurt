src=util.ml request_ctx.ml hdr.ml route.ml hoof.ml
flags=-I `ocamlfind query uri` \
	  -I `ocamlfind query conduit` \
	  -I `ocamlfind query lwt` \
	  -I `ocamlfind query cohttp.lwt`

lib.byte:
	ocamlc $(flags) -a -o hoof.cma $(src)

lib.native:
	ocamlopt $(flags) -a -o  hoof.cmxa $(src)

all: lib.native lib.byte

install: all
	$(MAKE) uninstall || :
	ocamlfind install hoof META hoof.cmxa hoof.cma *.cmx *.cmo *.cmi *.o hoof.a

uninstall:
	ocamlfind remove hoof

clean:
	rm -f *.cm* *.o *.a

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
