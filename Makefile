MODULES := Latex LatexAst_helper LatexParser LatexAst_iterator
SOURCES := $(addsuffix .ml,$(MODULES))
DEPENDS := $(addsuffix .depends,$(SOURCES))
OBJECTS := $(SOURCES:.ml=.cmx)

PACK := -package Typography,earley,earley_ocaml -I +compiler-libs

OCAMLFIND := ocamlfind
OCAMLC   = $(OCAMLFIND) ocamlc $(if $(OCPP),-pp '$(OCPP)',) $(PACK)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(if $(OCPP),-pp '$(OCPP)',) $(PACK)
OCAMLDEP = $(OCAMLFIND) ocamldep $(if $(OCPP),-pp '$(OCPP)',) $(OCAMLDEPEXTRAS)
PA_OCAML := pa_ocaml

OCAMLFIND_IGNORE_DUPS_IN := $(shell $(OCAMLFIND) query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

.PHONY: all
all: patothez patothez.cmxa

patothez: patothez.cmxa patothez.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

patothez.cmxa: $(OBJECTS)
	$(OCAMLOPT) -a -o $@ $^

LatexParser.ml.depends LatexParser.cmx: private OCPP := $(PA_OCAML)

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.ml.depends: %.ml
	$(OCAMLDEP) $< > $@

include $(DEPENDS)
include patothez.ml.depends

.PHONY: clean
clean:
	rm -f *.cm[iox] *.depends *~ .*~ *.o *.cma *.cmxa *.a
