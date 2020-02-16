{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.Tuple";
    sources-directory = ./.;
    sources = [
      "Data.Tuple"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
