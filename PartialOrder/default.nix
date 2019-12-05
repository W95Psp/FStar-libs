(import <nixpkgs> {}).fstar-package-manager
  { name = "PartialOrder";
    sources-directory = ./.;
    sources = [
      "PartialOrder"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
