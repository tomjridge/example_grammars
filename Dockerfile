
FROM ocaml/opam2:4.07

# some of the following apt packages are likely already installed
RUN sudo apt-get install -y git make
RUN sudo apt-get install -y curl
RUN sudo apt-get install -y gcc
RUN sudo apt-get install -y bzip2
RUN sudo apt-get install -y wget
RUN sudo apt-get install -y unzip m4
RUN sudo apt-get install -y time
RUN sudo apt-get install -y rsync bubblewrap

RUN opam update

# install some common packages, so they are cached in future docker builds
RUN opam install dune ocamlfind odoc
RUN opam install core_kernel 
RUN opam install core
RUN opam install re

RUN opam pin add -y -n p0_lib https://github.com/tomjridge/p0.git#dev
RUN opam pin add -y -n example_grammars https://github.com/tomjridge/example_grammars.git
RUN opam install -y p0_lib example_grammars


