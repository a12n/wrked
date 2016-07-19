OCAMLBUILD_FLAGS ?=
OCAMLBUILD_FLAGS += -use-ocamlfind

OCAML_LIB_DIR != ocamlc -where

include $(OCAML_LIB_DIR)/Makefile.config

.PHONY: lib clean install utop

lib:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk.cma wrk.cmxa

clean:
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean
	rm -f qtest.targets.log

install: lib
	ocamlfind install wrk	\
		META	\
		_build/wrk$(EXT_LIB)	\
		_build/wrk.cma	\
		_build/wrk.cmi	\
		_build/wrk.cmxa

uninstall:
	ocamlfind remove wrk

test:

utop: lib
	utop -I _build/
