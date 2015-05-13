git clone -b dev https://github.com/tomjridge/e3.git
git clone -b dev https://github.com/tomjridge/p1.git
git clone -b dev https://github.com/tomjridge/p4.git
git clone https://github.com/tomjridge/example_grammars.git

# to build via nix
function build_with_nix {
  cd example_grammars && nix-build
}


# to build locally
function build_locally {
  cd example_grammars && ln -sf local/Makefile.local . && cd ..
  cd p4 && ln -sf ../example_grammars/Makefile.local . && cd ..
  cd example_grammars && local/make.sh && cd ..
  cd example_grammars && make
}

build_with_nix

