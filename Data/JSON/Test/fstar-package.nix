{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.JSON.Test";
    sources-directory = ./.;
    sources = [
      "Data.JSON.Test"
    ];
    ocaml-sources = [];
    dependencies =
      with (import ./../../..);
      [ Data.JSON ];
    compile = [];
  }
