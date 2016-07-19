OCAMLBUILD_FLAGS ?=
OCAMLBUILD_FLAGS += -use-ocamlfind

OCAML_LIB_DIR != ocamlc -where

include $(OCAML_LIB_DIR)/Makefile.config

.PHONY: app lib clean install-app install-lib test uninstall-lib utop

lib:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk.cma wrk.cmxa

app:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk2il.native

clean:
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean
	rm -f qtest.targets.log

install-app: app
	install -D -s wrk2il.native $(DESTDIR)/bin/wrk2il

install-lib: lib
	ocamlfind install wrk	\
		META	\
		_build/wrk$(EXT_LIB)	\
		_build/wrk.cma	\
		_build/wrk.cmi	\
		_build/wrk.cmx	\
		_build/wrk.cmxa

uninstall-lib:
	ocamlfind remove wrk

test:
	ocamlbuild $(OCAMLBUILD_FLAGS) test.byte
	@./test.byte

utop: lib
	utop -I _build/
