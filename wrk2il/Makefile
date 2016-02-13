OCAMLBUILD_FLAGS ?=
OCAMLBUILD_FLAGS += -cflag -safe-string
OCAMLBUILD_FLAGS += -cflags -w,+A-32-34-44
OCAMLBUILD_FLAGS += -use-ocamlfind

.PHONY: all app clean install test top

all: app

app:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk2il.native

clean:
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean

install: app
	install -D -s wrk2il.native $(DESTDIR)/bin/wrk2il

test:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk2il_test.byte
	@./wrk2il_test.byte

top: app
	utop -I _build/ -require batteries
