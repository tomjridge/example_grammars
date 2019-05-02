{ }: 
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    p1 = import ./../p1 { }; 
    e3 = import ./../e3 { }; 
    p4 = import ./../p4 { }; 
    ocaml=pkgs.ocaml_4_02_1; 
    findlib=pkgs.ocamlPackages_4_02_1.findlib;
    sexplib=pkgs.ocamlPackages_4_02_1.ocaml_sexplib;
    cppo=pkgs.ocamlPackages_4_02_1.cppo;
in stdenv.mkDerivation {
      name = "example_grammars";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60p46482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib cppo sexplib e3 p1 p4 ];
    
      configurePhase = "true"; 	# Skip configure

#      patchPhase = "ln -sf ${e3} src_ext/e3";

#      buildPhase="cd build && make && cd ..";
 
      installPhase = "mkdir -p $out && cp -R * $out"; # so we can inspect the result
    
      createFindlibDestdir = true;
    }
