(import <nixpkgs> {}).fstar-package-manager
  { name = "Data.Tuple";
    sources-directory = ./.;
    sources = [
      "Data.Tuple"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
