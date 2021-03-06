MODULES := Latex LatexAst_helper LatexParser LatexAst_iterator
SOURCES := $(addsuffix .ml,$(MODULES))
DEPENDS := $(addsuffix .depends,$(SOURCES))
OBJECTS := $(SOURCES:.ml=.cmo)
OBJECTS_OPT := $(SOURCES:.ml=.cmx)

PACK := -package Typography,earley,earley_ocaml -I +compiler-libs

OCAMLFIND := ocamlfind
OCAMLC   = $(OCAMLFIND) ocamlc   $(if $(OCPP),-pp '$(OCPP)',) $(PACK)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(if $(OCPP),-pp '$(OCPP)',) $(PACK)
OCAMLDEP = $(OCAMLFIND) ocamldep $(if $(OCPP),-pp '$(OCPP)',) $(OCAMLDEPEXTRAS)
PA_OCAML := pa_ocaml

OCAMLFIND_IGNORE_DUPS_IN := $(shell $(OCAMLFIND) query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

.PHONY: all
all: patotex patotex.cma patotex.cmxa

patotex: patotex.cmxa patotex.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

patotex.cma: $(OBJECTS)
	$(OCAMLC) -a -o $@ $^

patotex.cmxa: $(OBJECTS_OPT)
	$(OCAMLOPT) -a -o $@ $^

LatexParser.ml.depends LatexParser.cmx LatexParser.cmo: private OCPP := $(PA_OCAML)

%.cmo: %.ml
	$(OCAMLC) -o $@ -c $<

%.cmx: %.ml
	$(OCAMLOPT) -o $@ -c $<

%.ml.depends: %.ml
	$(OCAMLDEP) $< > $@

include $(DEPENDS)
include patotex.ml.depends

.PHONY: clean
clean:
	rm -f *.cm[iox] *.depends *~ .*~ *.o *.cma *.cmxa *.a
