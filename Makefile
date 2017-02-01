clean:
	rm -rf _build

docs:
	mkdir -p doc
	ocamldoc -d doc $(src) -html
