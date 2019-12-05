(import <nixpkgs> {}).fstar-package-manager
  { name = "ToString";
    sources-directory = ./.;
    sources = [
      "ToString"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
