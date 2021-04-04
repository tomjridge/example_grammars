SHELL:=bash

default: all

-include Makefile.ocaml

run_test:
	dune exec bin/test.exe

test_scala_example:
	dune exec bin/test.exe 4

test_interactive:
	cd src && ocaml -init .ocamlinit # then #use "scala_example.utop";;

# install:
# 	dune install
# 
# # run_example:
# # 	dune exec bin/p0_example.exe
# 
# BUILD_DOC:=_build/default/_doc/_html
# TMP:=/tmp/example_grammars
# docs: FORCE
# 	dune build @doc
# 	mkdir -p $(TMP)
# 	rsync -vaz $(BUILD_DOC)/* $(TMP)
# 
# promote_docs: FORCE
# 	rm -rf docs/*
# 	cp -R $(BUILD_DOC)/* docs
# 
# clean:
# 	dune clean

clean::

FORCE:
