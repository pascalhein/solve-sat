#Christian
#
#Unittests kann ich auf Wunsch einpflegen.
#
#gmake

ARCHIVES_TEST = data.cma options.cma io.cma up.cma #test_up.cmo
ARCHIVES = data.cma options.cma io.cma up.cma select.cma # order matters!
ARCHIVES_NATIVE = $(ARCHIVES:%.cma=%.cmxa)
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS ?= -g

.PHONY: all check clean

all: solver native check

check: test
	./test
	@echo " "

test: $(ARCHIVES_TEST) test_up.ml
	ocamlfind $(OCAMLC) ${OCAMLFLAGS} -o test -package oUnit -package str -linkpkg $+ 

solver: $(ARCHIVES) main.ml
	ocamlfind $(OCAMLC) ${OCAMLFLAGS} -o $@ -package str -linkpkg $+

native: $(ARCHIVES_NATIVE) main.ml
	ocamlfind $(OCAMLOPT) ${OCAMLFLAGS} -o $@ -package str -linkpkg $+

%.cmo: %.ml
	$(OCAMLC) -c $*.ml
%.cma: %.cmo
	$(OCAMLC) -a -o $*.cma $*.cmo
%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml
%.cmxa: %.cmx
	$(OCAMLOPT) -a -o $*.cmxa $*.cmx

clean:
	rm -f *.cmo *.cma *.cmi *.cmx *.cmxa
	rm -f *.a *.o
	rm -f test solver native
