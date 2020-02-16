{nixpkgs ? import <nixpkgs> {}}:
  { name = "Debug.Test";
    sources-directory = ./.;
    sources = [
      "Debug.Test"
    ];
    ocaml-sources = [];
    dependencies = [
      (import ./..)
    ];
    compile = [];
  }
