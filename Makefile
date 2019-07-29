SHELL:=bash

build:
	dune build @install
	dune exec bin/test.exe

install:
	dune install

# run_example:
# 	dune exec bin/p0_example.exe

BUILD_DOC:=_build/default/_doc/_html
TMP:=/tmp/example_grammars
docs: FORCE
	dune build @doc
	mkdir -p $(TMP)
	rsync -vaz $(BUILD_DOC)/* $(TMP)

promote_docs: FORCE
	rm -rf docs/*
	cp -R $(BUILD_DOC)/* docs

clean:
	dune clean

FORCE:
