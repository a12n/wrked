OCAMLBUILD_FLAGS ?=
OCAMLBUILD_FLAGS += -cflag -safe-string
OCAMLBUILD_FLAGS += -cflags -w,+A-32-34-44
OCAMLBUILD_FLAGS += -use-ocamlfind

.PHONY: all app clean test top

all: app

app:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk2ir.byte

clean:
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean

test:
	ocamlbuild $(OCAMLBUILD_FLAGS) wrk2ir_test.byte
	@./wrk2ir_test.byte

top: app
	utop -I _build/ -require batteries
