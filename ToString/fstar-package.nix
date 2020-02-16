{nixpkgs ? import <nixpkgs> {}}:
  { name = "ToString";
    sources-directory = ./.;
    sources = [
      "ToString"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
