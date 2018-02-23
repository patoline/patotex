SOURCES := thezformat.ml latexSyntax.ml parser.dyp generateur.ml patothez.ml
PACK_TYPO := -package Typography,Typography.FormatArticle,camlp4.fulllib,dynlink
PACK_DYP := -package dyp
PACK := $(PACK_TYPO) $(PACK_DYP)
CAMLC := ocamlfind ocamlopt $(PACK)
DYPGEN := dypgen --no-mli
CAMLP4OF := camlp4of

all: patothez thezformat.cmxa

SOURCES2 := $(SOURCES:.dyp=.ml)
OBJS := $(SOURCES2:.ml=.cmx)

parser.cmx: parser.ml latexSyntax.cmx thezformat.cmx
	echo Make parser
	$(CAMLC) -c $<

generateur.cmx: generateur.ml latexSyntax.cmx parser.cmx thezformat.cmxa
	$(CAMLC) -package camlp4 -pp $(CAMLP4OF) -c $<
patothez.cmx: latexSyntax.cmx generateur.cmx parser.cmx
thezformat.cmxa: thezformat.ml
	$(CAMLC) -a -o thezformat.cmxa

patothez: $(OBJS)
	$(CAMLC) -linkpkg -o patothez $(OBJS)

.SUFFIXES: .ml .cmx .dyp

%.cmx: %.ml
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f patothez
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli

thez.tml: thanks.tex thez.tex patothez
	./patothez < thez.tex
thez.opt: thez.tml
	ocamlfind ocamlopt -o thez.opt -I . -package Typography,Typography.FormatArticle,Typography.Pdf -linkpkg thezformat.cmx -impl thez.tml
thez.pdf: thez.opt
	./thez.opt
