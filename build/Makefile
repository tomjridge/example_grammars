SRC_DIR:=../src
MLS_TO_EXEC:= grammar.ml eee.ml eee2.ml #
MLS_FOR_LIB:=dummy.ml
CMO=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=lib
P1_GEN:=../src_ext/p1/build/p1_gen.native

CAMLCINCLUDES:=str.cma -I ../src_ext/p1/build p1.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS:=-w -8 #-w -26-8
COMPFLAGS:=$(WARNINGS)

all: $(P1_GEN) before_all 
	make grammar.native
	make x_to_g
	make g_to_ml
	make cmo
	make $(MLS_TO_EXEC:.ml=.native)

cmo: $(CMO)

$(P1_GEN): 
	cd ../src_ext/p1/build && make

P1X:=$(wildcard *.p1x)
x_to_g: $(P1X:.p1x=.p1)

GS:=$(wildcard *.p1)
g_to_ml: $(GS:.p1=.ml) 

include Makefile.include

.SUFFIXES: .p1 .p1x
.p1.ml:
	$(P1_GEN) -g $< > $@

.p1x.p1: 
	./grammar.native $< > $@

clean:
	rm -f $(GS:.p1=.ml) *.p1x *.txt
	rm -f $(GS)
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend
	rm -rf ocamldoc

FORCE:

