{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.Function";
    sources-directory = ./.;
    sources = [
      "Data.Function"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
