SRC_DIR:=../src
MLS_TO_EXEC:= grammar.ml #
IGNORE_FOR_LIB:= grammar.ml interactive.ml # don't want these in the lib
MLS_FOR_LIB_1=$(shell ocamlfind ocamldep -sort *.ml)
MLS_FOR_LIB=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB_1)) # p1_examples.ml is an executable
CMO=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=lib
P1_GEN:=../../src_ext/p1/build/p1_gen.native

CAMLCINCLUDES:=str.cma -I ../../src_ext/p1/build p1.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS:=-w -8 #-w -26-8
COMPFLAGS:=$(WARNINGS)

all: $(P1_GEN) before_all g_to_ml
	make cmo
	make $(MLS_TO_EXEC:.ml=.native)

cmo: $(CMO)

$(P1_GEN): 
	cd ../../src_ext/p1/build && make

GS=$(wildcard *.g)
g_to_ml: $(GS:.g=.ml)

include Makefile.include

.SUFFIXES: .g
.g.ml:
	$(P1_GEN) -g $< > $@

clean:
	rm -f $(GS:.g=.ml)
	rm -f $(GS)
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend
	rm -rf ocamldoc

FORCE:

