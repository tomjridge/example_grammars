SHELL:=bash

CAMLC:=ocamlfind ocamlc -linkpkg -package sexplib,sexplib.syntax -syntax camlp4o
CAMLOPT:=ocamlfind ocamlopt -linkpkg -package sexplib,sexplib.syntax -syntax camlp4o
CAMLDEP:=ocamlfind ocamldep -package sexplib,sexplib.syntax -syntax camlp4o -one-line
OCAMLDOC:=ocamlfind ocamldoc

#OREBUILD:=./orebuild

COMPFLAGS?:=# maybe set some additional compilation options

SRC_DIR:=../src
MLS_TO_EXEC:= grammar.ml eee.ml eee2.ml eee3_a1.ml eee3_a2.ml ocaml_p1.ml ocaml_p4.ml abnf_grammar.ml abnf_grammar_2.ml abnf_grammar_main.ml #
MLS_FOR_LIB:=abnf_grammar_p4.ml
MLS:=$(cat depend)
CMO:=$(MLS:.ml=.cmo)
LIB:=example_grammars

P1_GEN:=../src_ext/p1/build/p1_gen.native
P1_GEN_P4:=../src_ext/p1/build/p1_gen_p4.native

CAMLCINCLUDES:=str.cma -I ../src_ext/p1/build p1.cma -I ../src_ext/e3/build e3.cma -I ../src_ext/p4/build p4.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS:=-w -8 #-w -26-8
COMPFLAGS:=$(WARNINGS)

# turn off line numbering, which doesn't work so well with code generation 
CPPO:=cppo -n

default: all

../src_ext/p1/build/p1.cma ../src_ext/e3/build/e3.cma ../src_ext/p4/build/p4.cma:
	cd ../src_ext/p1/build && make
	cd ../src_ext/e3/build && make
	cd ../src_ext/p4/build && make

before_all:
	make link 
	make grammar.native 
	make ocaml_p1.p1x ocaml_p4.p4x eee3_a1.p1 eee3_a2.p1
	make mls
	make depend

link: $(SRC_DIR)
	find $(SRC_DIR) -maxdepth 2 -type f -exec ln -sf \{\} . \;

depend:
	$(CAMLDEP) -sort *.ml *.mli >depend # costly

all: ../src_ext/p1/build/p1.cma ../src_ext/e3/build/e3.cma ../src_ext/p4/build/p4.cma before_all 
	make $(MLS_TO_EXEC:.ml=.native)

# want to build all ml files prior to generating dependency info; need
# all the .p1 .p1x .p4 .p4x; but some of them (like eee3.p1x) aren't
# directly linked to eee3.ml; so we appear to need to explicitly give
# some of these targets; related is that we have some files with the
# same extension that need to be processed in different ways (eee3.p1x
# again)
mls: $(patsubst %.p1,%.ml,$(wildcard *.p1)) \
	$(patsubst %.p1x,%.ml,$(wildcard *.p1x)) \
  $(patsubst %.p4,%.ml,$(wildcard *.p4)) \
	$(patsubst %.p4x,%.ml,$(wildcard *.p4x))


tmp: 
	make before_all
	make grammar.native
	make abnf_grammar.native
	make abnf_grammar_2.native
	make abnf_grammar_main.native

test:
	./ocaml_p1.native os_command.lem
	./ocaml_p4.native os_command.lem


ocaml_p1.p1x: ocaml.cppo
	$(CPPO) ocaml.cppo >$@

ocaml_p4.p4x: ocaml.cppo
	$(CPPO) -D P4 ocaml.cppo >$@


eee3_a1.p1: eee3.p1x.acts grammar.native
	rm -f $@
	./grammar.native -tag a1 $< >$@
	chmod u-w $@

eee3_a2.p1: eee3.p1x.acts grammar.native
	rm -f $@
	./grammar.native -tag a2 $< >$@
	chmod u-w $@

cmx: $(MLS) 
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $(MLS)
	touch cmx

$(LIB).cmxa: cmx
	$(CAMLOPT) $(COMPFLAGS) -a -o $@ $(MLS_FOR_LIB:.ml=.cmx)


$(MLS_TO_EXEC:.ml=.native): %.native: %.ml $(LIB).cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ $(LIB).cmxa $<




.SUFFIXES: .ml .p1 .p1x .p4x .p4
.p1x.p1: 
	rm -f $@
	./grammar.native $< > $@
	chmod u-w $@

.p1.ml:
	rm -f $@
	$(P1_GEN) -g $< > $@
	chmod u-w $@

.p4x.p4:
	rm -f $@
	./grammar.native -assoc left $< > $@
	chmod u-w $@

.p4.ml:
	rm -f $@
	$(P1_GEN_P4) -g $< > $@
	chmod u-w $@


.PRECIOUS: %.p1 %.ml %.p4

clean:
	rm -f $(GS:.p1=.ml) *.cppo *.p4 *.p4x *.p1x *.p1 *.txt *.lem
	rm -f $(GS)
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend depend cmo
	rm -f *.org 
	rm -rf ocamldoc orebuild.exe

FORCE:

