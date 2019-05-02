SHELL:=bash

build:
	dune build @install

install:
	dune install

# run_example:
# 	dune exec bin/p0_example.exe

BUILD_DOC:=_build/default/_doc/_html
TMP:=/tmp/p0
docs: FORCE
	dune build @doc
	mkdir -p $(TMP)
	rsync -vaz $(BUILD_DOC)/* $(TMP)

promote_docs: FORCE
	rm -rf docs/*
	cp -R $(BUILD_DOC)/* docs

clean:
	dune clean
	rm -f p0_lib.install

FORCE:
