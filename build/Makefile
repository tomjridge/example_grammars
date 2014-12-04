SRC_DIR:=../src
MLS_TO_EXEC:= grammar.ml eee.ml eee2.ml eee3.ml ocaml.ml ocaml_p4.ml #
MLS_FOR_LIB:=dummy.ml
CMO=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=lib
P1_GEN:=../src_ext/p1/build/p1_gen.native
P1_GEN_P4:=../src_ext/p1/build/p1_gen_p4.native

CAMLCINCLUDES:=str.cma -I ../src_ext/p1/build p1.cma -I ../src_ext/e3/build e3.cma -I ../src_ext/p4/build p4.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS:=-w -8 #-w -26-8
COMPFLAGS:=$(WARNINGS)

# turn off line numbering, which doesn't work so well with code generation 
CPPO:=cppo -n

all: $(P1_GEN) ../src_ext/p1/build/p1.cma ../src_ext/e3/build/e3.cma ../src_ext/p4/build/p4.cma before_all 
	make grammar.native
	make cppo_to
	make p1x_to_p1
	make p4x_to_p4
	make p1_to_ml
	make ocaml_p4.ml
	make cmo
	make $(MLS_TO_EXEC:.ml=.native)

../src_ext/p1/build/p1.cma ../src_ext/e3/build/e3.cma ../src_ext/p4/build/p4.cma:
	cd ../src_ext/p1/build && make
	cd ../src_ext/e3/build && make
	cd ../src_ext/p4/build && make

test:
	./ocaml.native os_command.lem
	./ocaml_p4.native os_command.lem

cmo: $(CMO)

$(P1_GEN): 
	cd ../src_ext/p1/build && make

ocaml.p1x: ocaml.cppo
	$(CPPO) ocaml.cppo >ocaml.p1x

ocaml.p4x: ocaml.cppo
	$(CPPO) -D P4 ocaml.cppo >ocaml.p4x

cppo_to: ocaml.p1x ocaml.p4x


P1X:=$(wildcard *.p1x)
p1x_to_p1: $(P1X:.p1x=.p1)

P1S:=$(wildcard *.p1)
p1_to_ml: $(P1S:.p1=.ml) 

P4X:=$(wildcard *.p4x)
p4x_to_p4: $(P4X:.p4x=.p4)

ocaml_p4.ml: ocaml.p4
	rm -f $@
	$(P1_GEN_P4) -g $< > $@ 
	chmod u-w $@

include Makefile.include

eee3.p1: eee3.p1x grammar.native
	rm -f $@
	./grammar.native -tag a2 $< >$@
	chmod u-w $@

.SUFFIXES: .p1 .p1x .p4x .p4
.p1.ml:
	rm -f $@
	$(P1_GEN) -g $< > $@
	chmod u-w $@

.p1x.p1: 
	rm -f $@
	./grammar.native $< > $@
	chmod u-w $@

.p4x.p4:
	rm -f $@
	./grammar.native -assoc left $< > $@
	chmod u-w $@


clean:
	rm -f $(GS:.p1=.ml) *.cppo *.p4 *.p4x *.p1x *.p1 *.txt *.lem
	rm -f $(GS)
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend
	rm -rf ocamldoc

FORCE:

