(import <nixpkgs> {}).fstar-package-manager
  { name = "DefaultValue";
    sources-directory = ./.;
    sources = [
      "DefaultValue"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
