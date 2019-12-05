(import <nixpkgs> {}).fstar-package-manager
  { name = "Data.Function";
    sources-directory = ./.;
    sources = [
      "Data.Function"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
