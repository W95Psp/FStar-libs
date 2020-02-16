{nixpkgs ? import <nixpkgs> {}}: 
  { name = "DefaultValue";
    sources-directory = ./.;
    sources = [
      "DefaultValue"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
